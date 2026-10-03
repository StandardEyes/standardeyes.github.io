# Third-party notices

StandardEyes itself is released under the MIT License (see LICENSE). The release ZIP
bundles the following components, each under its own licence. The licence texts are in
the `licenses/` folder of the ZIP: `python/` (CPython), `tcl-tk/`, `python-packages/<package>/`,
`R-packages/<package>/`, `javascript/` and `common/` (GPL, LGPL, Apache and the MIT / BSD
templates that R packages licensed "MIT + file LICENSE" complete with their own LICENSE file).

## Bundled Python runtime

- CPython 3.12.10 (Windows x64, a copy of the python.org installer layout in runtime/python) - PSF License Version 2 (`licenses/python/CPython-LICENSE.txt`; https://docs.python.org/3/license.html)
- Tcl/Tk 8.6 - Tcl/Tk License (BSD-style; `licenses/tcl-tk/`)

## Python packages (runtime/python/Lib/site-packages)

| Package | Version | Licence |
|---|---|---|
| annotated-doc | 0.0.4 | MIT |
| annotated-types | 0.7.0 | MIT |
| anyio | 4.14.1 | MIT |
| click | 8.4.2 | BSD-3-Clause |
| colorama | 0.4.6 | BSD |
| et_xmlfile | 2.0.0 | MIT |
| fastapi | 0.139.0 | MIT |
| fonttools | 4.63.0 | MIT |
| h11 | 0.16.0 | MIT |
| httptools | 0.8.0 | MIT |
| idna | 3.18 | BSD-3-Clause |
| numpy | 2.5.1 | BSD-3-Clause AND 0BSD AND MIT AND Zlib AND CC0-1.0 |
| openpyxl | 3.1.5 | MIT |
| packaging | 26.2 | Apache-2.0 OR BSD-2-Clause |
| pandas | 3.0.3 | BSD 3-Clause License |
| pydantic | 2.13.4 | MIT |
| pydantic_core | 2.46.4 | MIT |
| python-dateutil | 2.9.0.post0 | Apache-2.0 OR BSD-3-Clause (dual licence) |
| python-dotenv | 1.2.2 | BSD-3-Clause |
| python-multipart | 0.0.32 | Apache-2.0 |
| PyYAML | 6.0.3 | MIT |
| six | 1.17.0 | MIT |
| starlette | 1.3.1 | BSD-3-Clause |
| typing_extensions | 4.16.0 | PSF-2.0 |
| typing-inspection | 0.4.2 | MIT |
| tzdata | 2026.3 | Apache-2.0 |
| uvicorn | 0.51.0 | BSD-3-Clause |
| watchfiles | 1.2.0 | MIT |
| websockets | 16.1 | BSD-3-Clause |

## Native libraries inside the Python packages

- OpenBLAS (numpy.libs, `libscipy_openblas*.dll`) - BSD-3-Clause; text in `licenses/python-packages/numpy-2.5.1/LICENSE.txt`
- Microsoft Visual C++ runtime (`msvcp140*.dll` in numpy.libs) - redistributed under the Microsoft Visual C++ Redistributable licence

## Vendored JavaScript (webapp/static/vendor)

- Alpine.js 3.14.1 - MIT License (`licenses/javascript/Alpine.js-LICENSE.txt`; https://github.com/alpinejs/alpine)

## R packages (runtime/Rlib)

| Package | Version | Licence |
|---|---|---|
| MatchIt | 4.7.2 | GPL (>= 2) |
| R6 | 2.6.1 | MIT + file LICENSE |
| RColorBrewer | 1.1-3 | Apache License 2.0 |
| Rcpp | 1.1.0 | GPL (>= 2) |
| RcppProgress | 0.4.2 | GPL (>= 3) |
| S7 | 0.2.1 | MIT + file LICENSE |
| backports | 1.5.0 | GPL-2 \| GPL-3 |
| broom | 1.0.11 | MIT + file LICENSE |
| cellranger | 1.1.0 | MIT + file LICENSE |
| chk | 0.10.0 | MIT + file LICENSE |
| cli | 3.6.5 | MIT + file LICENSE |
| cobalt | 4.6.1 | GPL (>= 2) |
| cpp11 | 0.5.2 | MIT + file LICENSE |
| crayon | 1.5.3 | MIT + file LICENSE |
| dplyr | 1.1.4 | MIT + file LICENSE |
| farver | 2.1.2 | MIT + file LICENSE |
| geepack | 1.3.13 | GPL (>= 3) |
| generics | 0.1.4 | MIT + file LICENSE |
| ggplot2 | 4.0.1 | MIT + file LICENSE |
| glue | 1.8.0 | MIT + file LICENSE |
| gridExtra | 2.3 | GPL (>= 2) |
| gtable | 0.3.6 | MIT + file LICENSE |
| hms | 1.1.4 | MIT + file LICENSE |
| isoband | 0.3.0 | MIT + file LICENSE |
| jsonlite | 2.0.0 | MIT + file LICENSE |
| labeling | 0.4.3 | MIT + file LICENSE \| Unlimited |
| lifecycle | 1.0.4 | MIT + file LICENSE |
| magrittr | 2.0.4 | MIT + file LICENSE |
| openxlsx | 4.2.8.1 | MIT + file LICENSE |
| pillar | 1.11.1 | MIT + file LICENSE |
| pkgconfig | 2.0.3 | MIT + file LICENSE |
| prettyunits | 1.2.0 | MIT + file LICENSE |
| progress | 1.2.3 | MIT + file LICENSE |
| purrr | 1.2.0 | MIT + file LICENSE |
| readxl | 1.4.5 | MIT + file LICENSE |
| rematch | 2.0.0 | MIT + file LICENSE |
| rlang | 1.1.6 | MIT + file LICENSE |
| scales | 1.4.0 | MIT + file LICENSE |
| stringi | 1.8.7 | BSD-3-Clause; bundled ICU library under the ICU / Unicode licence (full text: licenses/R-packages/stringi/LICENSE) |
| stringr | 1.6.0 | MIT + file LICENSE |
| tibble | 3.3.0 | MIT + file LICENSE |
| tidyr | 1.3.2 | MIT + file LICENSE |
| tidyselect | 1.2.1 | MIT + file LICENSE |
| utf8 | 1.2.6 | Apache License (== 2.0) \| file LICENSE |
| vctrs | 0.6.5 | MIT + file LICENSE |
| viridisLite | 0.4.2 | MIT + file LICENSE |
| withr | 3.0.2 | MIT + file LICENSE |
| zip | 2.3.3 | MIT + file LICENSE |

The GPL-licensed R packages are redistributed unmodified as compiled binaries; the corresponding source for the exact bundled versions is available at https://cran.r-project.org/src/contrib/Archive/<package>/ (and on request from the author). GPL-licensed packages bundled: MatchIt, Rcpp, RcppProgress, backports, cobalt, geepack, gridExtra.
The GPL-2 and GPL-3 texts are in `licenses/common/`.

R itself is not bundled; the user installs R 4.5.x (GPL-2 | GPL-3) separately.
