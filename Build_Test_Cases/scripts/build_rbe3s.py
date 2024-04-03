import os
import numpy as np
from pyNastran.bdf.bdf import BDF, CaseControlDeck

dirname = os.path.dirname(__file__)

bdf_filename = os.path.join(dirname, 'rbe3s.bdf')

load_id = 2
spc_id = 3
cc_lines = [
    'SUBCASE 1',
    f'LOAD = {load_id}',
    f'SPC = {spc_id}',
    'DISP(PRINT,PLOT)=ALL',
    'MPCFORCE(PRINT,PLOT)=ALL',
    'SPCFORCE(PRINT,PLOT)=ALL',
]

xs = np.linspace(0., 1., num=5)
ys = xs
zs = xs
model = BDF()
model.sol = 101
model.case_control_deck = CaseControlDeck(cc_lines)

eid = 1
pid = 2
mid = 3
model.add_grid(1, [0., 0., -1.])
model.add_grid(2, [1., 0., -1.])
model.add_crod(eid, pid, [1, 2])
model.add_prod(pid, mid, 1.0, j=1.0)
model.add_mat1(mid, 3.0e7, None, 0.3)
nid0 = 10

z = 0.
for xi in xs:
    for yi in ys:
        for zi in zs:
            if xi == yi and xi == zi:
                continue
            model.add_grid(nid0+1, [0., 0., z])
            model.add_grid(nid0+2, [1., 0., z])
            model.add_grid(nid0+3, [1., 1., z])
            model.add_grid(nid0+4, [0., 1., z])
            model.add_grid(nid0+5, [0.5, 0.5, z])
            refc = '123456'
            comps = ['123'] * 4
            weights = [1.0] * 4
            Gijs = [nid0+1, nid0+2, nid0+3, nid0+4]
            refgrid = nid0+5
            model.add_rbe3(eid, refgrid, refc, weights, comps, Gijs, Gmi=None, Cmi=None, alpha=0.0, tref=0.0, comment='')
            model.add_spc1(spc_id, '123456', Gijs, comment='')

            xyz = [xi, yi, zi]
            model.add_force(load_id, refgrid, 1.0, xyz, cid=0, comment='')
            eid += 5
            nid0 += 5
            z += 5.
model.write_bdf(bdf_filename, enddata=True)
x = 1
