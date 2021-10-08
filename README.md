# How to test

```
docker run --rm -it \
    -v $(pwd):/home/runner/work \
    -v ${HOME}/data/renv_cache:/mnt/shared/renv/cache \
    -w /home/runner/work \
    -e R_LIBS_="~/.local/share/rlibs" \
    -e RENV_PATHS_ROOT="~/.local/share/renv" \
    -e RENV_PATHS_CACHE="/mnt/shared/renv/cache" \
    rocker/r-ver:4.0.3 \
    bash 
```

Install system packages
```bash
apt update && apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev
```

```bash
R -e 'chooseCRANmirror(ind=1)
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()'
```

```bash
Rscript ticker.R

Rscript ./naver.R

Rscript ./fnguide.R
```
