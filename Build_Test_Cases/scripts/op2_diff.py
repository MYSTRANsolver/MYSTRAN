import os
import shutil
import subprocess
from pathlib import Path
import numpy as np
import pandas as pd
from pyNastran.bdf.bdf_interface.assign_type_force import parse_components, parse_components_or_blank

import pyNastran.bdf.bdf_interface.assign_type
pyNastran.bdf.bdf_interface.assign_type.parse_components = parse_components
pyNastran.bdf.bdf_interface.assign_type.parse_components_or_blank = parse_components_or_blank

from pyNastran.utils.dev import get_files_of_type


def process_op2s_compare(baseline_op2_filenames,
                         op2_filenames,
                         convert_op2_to_excel=True):
    from pyNastran.op2.op2 import read_op2
    #from pyNastran.f06.csv_writer import write_csv
    build_dataframe = convert_op2_to_excel

    assert len(baseline_op2_filenames) == len(op2_filenames)
    for baseline_op2_filename, op2_filename in zip(baseline_op2_filenames, op2_filenames):
        if not os.path.exists(op2_filename):
            print(f'*{op2_filename}')
            continue
        base = os.path.splitext(op2_filename)[0]
        #excel_filename = base + '.xlsx'
        #csv_filename = base + '.csv'
        try:
            model = read_op2(op2_filename, build_dataframe=build_dataframe, debug=False)
        except Exception as e:
            print(f'***{op2_filename}')
            continue

        if os.path.getsize(baseline_op2_filename) == 0:
            print(f'***{baseline_op2_filename}')
            continue

        model_baseline = read_op2(baseline_op2_filename, build_dataframe=build_dataframe, debug=False)
        rtol = 1.e-5
        atol = 1.e-8
        is_passed = is_op2_close(model_baseline, model, rtol=rtol, atol=atol)
        if not is_passed:
            print(f'*{op2_filename}')
            y = 1
        x = 1

        #if convert_op2_to_excel:
        #op2_to_excel(model, excel_filename)
    return

def is_op2_close(model_baseline, model,
                 rtol: float=1.e-5,
                 atol: float=1.e-8) -> bool:
    is_passed = True
    #op2_filename = model.op2_filename
    log = model.log
    for datai in get_op2_results(model):
        if len(datai) == 2:
            table_type, result = datai
        else:
            assert len(datai) == 3
            # grid_point_weight
            table_type, key, obj = datai
            baseline_dict = getattr(model_baseline, table_type)
            if table_type == 'grid_point_weight' and key not in baseline_dict:
                #log.warning('no grid_point_weight')
                continue
            baseline_obj = baseline_dict[key]
            assert obj == baseline_obj, obj - baseline_obj
            continue

        baseline_result = model_baseline.get_result(table_type)
        for subcase_id, resulti in result.items():
            if subcase_id in baseline_result:
                baseline_resulti = baseline_result[subcase_id]
            else:
                if table_type == 'eigenvectors' and subcase_id in model_baseline.displacements:
                    baseline_resulti = model_baseline.displacements[subcase_id]
                elif subcase_id not in baseline_result:
                    is_passed = False
                    log.info(f'**{table_type} {subcase_id} is missing')
                    continue
                else:
                    raise RuntimeError('asdf')
                    #baseline_resulti = baseline_result[subcase_id]

            if hasattr(resulti, 'node_layer'):
                assert np.array_equal(baseline_resulti.node_layer, resulti.node_layer)
            if hasattr(resulti, 'element_node'):
                assert np.array_equal(baseline_resulti.element_node, resulti.element_node)
            if hasattr(resulti, 'element'):
                assert np.array_equal(baseline_resulti.element, resulti.element)
            if hasattr(resulti, 'node_gridtype'):
                assert np.array_equal(baseline_resulti.node_gridtype, resulti.node_gridtype)
            is_passedi = np.allclose(baseline_resulti.data, resulti.data,
                                     rtol=rtol, atol=atol, equal_nan=True)
            if not is_passedi:
                is_passed = False
                log.info(f'**{table_type}')
    return is_passed

def process_op2s(op2_filenames,
                 convert_op2_to_excel=True,
                 convert_op2_to_csv=True):
    from pyNastran.op2.op2 import read_op2
    from pyNastran.f06.csv_writer import write_csv
    build_dataframe = convert_op2_to_excel

    for op2_filename in op2_filenames:
        if not os.path.exists(op2_filename):
            print(f'*{op2_filename}')
            continue
        base = os.path.splitext(op2_filename)[0]
        excel_filename = base + '.xlsx'
        csv_filename = base + '.csv'
        try:
            model = read_op2(op2_filename, build_dataframe=build_dataframe, debug=False)
        except Exception as e:
            print(f'***{op2_filename}')
            continue

        if convert_op2_to_excel:
            op2_to_excel(model, excel_filename)

        if convert_op2_to_csv:
            write_csv(model, csv_filename, is_exponent_format=True)

def get_op2_results(model):
    log = model.log
    for table_type in model.get_table_types():
        if table_type in {'psds'}:
            continue
        result = model.get_result(table_type)
        if result is None or result == {}:
            continue
        if table_type in {'grid_point_weight'}:
            for key, weight in model.grid_point_weight.items():
                yield table_type, key, weight
            #log.warning(f'skipping {table_type}')
            continue
        yield table_type, result

def op2_to_excel(model, excel_filename) -> None:
    sheet_names = []
    pd_results = []
    for table_type, result in get_op2_results(model):
    #for table_type in model.get_table_types():
        #if table_type in {'psds'}:
            #continue
        #result = model.get_result(table_type)
        #if result is None or result == {}:
            #continue
        #if table_type in {'grid_point_weight'}:
            #log.warning(f'skipping {table_type}')
            #continue

        for subcase_id, resulti in result.items():
            if isinstance(subcase_id, tuple):
                subcase_id = subcase_id[0]

            base_sheet_name = resulti.class_name
            if base_sheet_name.startswith('Real'):
                base_sheet_name = base_sheet_name[4:]
            if base_sheet_name.endswith('Array'):
                base_sheet_name = base_sheet_name[:-5]
            sheet_name = f'S{subcase_id:d}_{base_sheet_name}'
            sheet_names.append(sheet_name)
            pd_results.append((sheet_name, resulti.dataframe))
    if pd_results:
        with pd.ExcelWriter(excel_filename) as writer:
            for sheet_name, dataframe in pd_results:
                dataframe.to_excel(writer, sheet_name=sheet_name)

def run_mystran_jobs(mystran_exe, bdf_filenames, run_mystran=True):
    bdf_filename0 = bdf_filenames[0]
    run_dirname = Path(os.path.dirname(bdf_filename0))
    os.chdir(run_dirname)

    op2_filenames = []
    f06_filenames = []
    assert os.path.exists(mystran_exe), mystran_exe
    for bdf_filename in bdf_filenames:
        base_dirname_base = os.path.basename(bdf_filename)
        base = os.path.splitext(bdf_filename)[0]
        op2_filename = base + '.op2'
        f06_filename = base + '.f06'
        f06_filenames.append(f06_filename)
        op2_filenames.append(op2_filename)
        args = [mystran_exe, base_dirname_base]
        assert os.path.exists(bdf_filename), args
        if not os.path.exists(op2_filename) and run_mystran:
            FNULL = open(os.devnull, 'w')
            return_code = subprocess.call(args, stdout=FNULL, stderr=FNULL)
            if return_code:
                print(bdf_filename, return_code)
    return op2_filenames

def add_plot_to_models(run_dirname, dat_filenames, add_op2_to_models=True):
    if add_op2_to_models:
        from pyNastran.bdf.bdf import read_bdf

    if not os.path.exists(run_dirname):
        os.makedirs(run_dirname)

    skip_cards = {'EIGR', 'EIGRL', 'PLOAD2', 'SPCADD', 'SPC', 'SPC1', 'LOAD'}
    bdf_filenames = []
    for dat_filename in dat_filenames:
        dat_filename_base = os.path.basename(dat_filename)
        bdf_filename_base = os.path.splitext(dat_filename_base)[0] + '.bdf'
        bdf_filename = run_dirname / bdf_filename_base
        if add_op2_to_models:
            model = read_bdf(
                dat_filename, validate=True, xref=True,
                skip_cards=skip_cards, read_cards=None,
                encoding=None, log=None, debug=None, mode='mystran')
            add_plot_to_case_control(model)
            model.write_bdf(bdf_filename)
        elif not os.path.exists(bdf_filename):
            bdf_filename = shutil.copyfile(dat_filename, bdf_filename)
        bdf_filenames.append(bdf_filename)
    return bdf_filenames

def add_plot_to_case_control(model) -> None:
    cc = model.case_control_deck
    string_keys = {'LABEL', 'SUBTITLE', 'TITLE', }
    int_keys = {'METHOD', 'SPC', 'MPC', 'LOAD'}
    skip_keys = {'ECHO', 'MEFFMASS', 'MPFACTOR', 'ELDATA', 'TEMPERATURE(LOAD)', 'TEMPERATURE(BOTH)', }
    keys_to_process = {'DISPLACEMENT', 'OLOAD', 'SPCFORCES', 'MPCFORCES',
                       'STRESS', 'STRAIN', 'FORCE', 'GPFORCE'}
    for subcase_id, subcase in cc.subcases.items():
        keys_to_update = []
        for key in subcase.params:
            if key in skip_keys or key in string_keys or key in int_keys:
                continue
            if key.startswith('SET '):
                continue
            if key in keys_to_process:
                keys_to_update.append(key)
                continue
            print(f'{key} is not supported')
            #assert key in keys_to_process, key
        for key in keys_to_update:
            value, options, res_type = subcase.params[key]
            if 'PLOT' not in options:
                options.append('PLOT')
            if 'PRINT' not in options:
                options.append('PRINT')
            subcase.params[key] = (value, options, res_type)

def main():
    base_dirname = Path(r'C:\mystran_bkp')
    add_op2_to_models = True
    convert_op2_to_csv = False
    convert_op2_to_excel = True
    run_mystran = True

    reference_dirname = base_dirname / 'Benchmark_Main_Package_12_29_2023'
    input_dirname     = reference_dirname / 'DAT' / 'Orig'
    #run_dirname       = reference_dirname / 'run_'
    #mystran_exe       = reference_dirname / 'MYSTRAN' / 'mystran-15.1.4.exe'

    dat_filenames = get_files_of_type(str(input_dirname), extension='.DAT')

    assert os.path.exists(input_dirname), input_dirname
    baseline_rev = '15.0'
    #revs = ['12.2', '15.0', '15.1', '15.1.1', '15.1.2', '15.1.3', '15.1.4']
    revs = ['15.0', '15.1.1']  # failed
    #revs = ['15.0', '15.1.4']  # failed
    revs = ['15.0', '15.9']  # failed
    #revs = ['15.9']  # dev
    bdf_filenames_by_rev = {}
    op2_filenames_by_rev = {}
    for rev in revs:
        mystran_exe = reference_dirname / 'MYSTRAN' / f'mystran-{rev}.exe'
        run_dirname = reference_dirname / f'run_{rev}'
        bdf_filenames = add_plot_to_models(run_dirname, dat_filenames, add_op2_to_models=add_op2_to_models)
        op2_filenames = run_mystran_jobs(mystran_exe, bdf_filenames, run_mystran=run_mystran)
        bdf_filenames_by_rev[rev] = bdf_filenames
        op2_filenames_by_rev[rev] = op2_filenames
        #process_op2s(op2_filenames,
                     #convert_op2_to_excel=convert_op2_to_excel,
                     #convert_op2_to_csv=convert_op2_to_csv)
    #for rev in revs:

    baseline_op2_filenames = op2_filenames_by_rev[baseline_rev]
    for rev in revs:
        if rev == baseline_rev:
            continue
        op2_filenames = op2_filenames_by_rev[rev]
        process_op2s_compare(baseline_op2_filenames,
                             op2_filenames,
                             convert_op2_to_excel=True)

    #for rev in revs:
    #baseline_run_dirname = reference_dirname / f'run_{baseline_rev}'


if __name__ == '__main__':
    main()
