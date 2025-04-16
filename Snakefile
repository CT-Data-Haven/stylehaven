from pathlib import Path

######################################
### FUNCTIONS                        ###
######################################
def flatten(xss):
    return [x for xs in xss for x in xs]


def switch_ext(path, ext):
    path = Path(path)
    return path.with_suffix(ext).name


def read_roxygen(path):
    with open(path, "r") as file:
        return [line.strip() for line in file if line.startswith("#'")]


def get_rd(path):
    path = Path(path)
    # read roxygen
    roxy = read_roxygen(path)
    # if has "@rdname" tag, use it
    # else use path stem
    rd = list(set([line for line in roxy if "@rdname" in line]))
    if len(rd) > 0:
        # return rd[0].split(' ')[2]
        return [r.split(" ")[2] for r in rd]
    else:
        return [path.stem]


######################################
### VARIABLES                        ###
######################################
pkg = 'stylehaven'

# scripts of functions that need tests
funcs = [
    f.stem for f in Path("R").glob("*.R") if f.stem not in ["data", f"{pkg}-package"]
]
vignettes = [f.name for f in Path('vignettes').glob('*.qmd')]

# scripts of functions that have docs
doc_funcs = [f for f in funcs if 'utils-' not in f]
# Rd files corresponding to documented functions' scripts
func_rds = flatten([get_rd(f"R/{func}.R") for func in doc_funcs])

datasets = ['cws_trend', 'life_exp', 'self_rated_health']

######################################
### RULES                            ###
######################################

# rule comments idea from https://lachlandeer.github.io/snakemake-econ-r-tutorial/self-documenting-help.html
## make_data: Pattern to connect datasets with their creation scripts
rule make_data:
    input:
        r = 'data-raw/make_{dataset}.R',
    output:
        rda = 'data/{dataset}.rda',
    shell:
        """
        Rscript '{input.r}'
        """

## data_raw: All data-creation rules
rule data_raw:
    input:
        expand('data/{dataset}.rda', dataset = datasets),

## tests: Run tests for all corresponding function scripts
rule tests:
    input:
        rules.data_raw.input,
        r=expand("R/{func}.R", func=funcs),
        test=expand("tests/testthat/test-{func}.R", func=funcs),
    shell:
        """
        Rscript -e "devtools::test()"
        """

## document: All documentation - package description, function docs, data docs, vignettes, readme
rule document:
    input:
        desc="DESCRIPTION",
        r=expand("R/{func}.R", func=doc_funcs),
        data=rules.data_raw.input,
        data_doc="R/data.R",
        pkg_doc=f"R/{pkg}-package.R",
        vignettes=expand("vignettes/{v}", v=vignettes),
        readme="README.md",
    output:
        func_rd = expand("man/{func}.Rd", func=func_rds),
        data_rd = expand('man/{dataset}.Rd', dataset = datasets),
        pkg_rd = f"man/{pkg}-package.Rd",
        namespace = "NAMESPACE",
    shell:
        'Rscript -e "devtools::document()"'

## check: devtools check of data creation, tests, package description
rule check:
    input:
        rules.data_raw.input,
        rules.tests.input.test,
        desc="DESCRIPTION",
    output:
        flag=touch(".flags/pkg-check.txt"),
    shell:
        """
        Rscript -e 'devtools::check(
            document = TRUE, 
            cran = FALSE,
            args = c(\"--run-dontrun\")
            )'
        """

## install: Test that package can install
rule install:
    input:
        data=rules.data_raw.input,
        check_flag=rules.check.output.flag,
        desc="DESCRIPTION",
    output:
        flag=touch(".flags/install"),
    shell:
        'Rscript -e "devtools::install()"'


## pkgdown: Test that site can build
rule pkgdown:
    input:
        "_pkgdown.yml",
        rules.document.output,
    output:
        flag=touch(".flags/pkgdown"),
    shell:
        'Rscript -e "pkgdown::build_site()"'

## render_quarto: Wildcard rule to render qmd files in any directory
# use constraint with regex to match readme in base of directory
rule render_quarto:
    input:
        qmd="{folder}{sep}{nb}.qmd",
    output:
        md="{folder}{sep}{nb}.md",
    wildcard_constraints:
        folder=".{0}|[a-z0-9-_]+",
        sep="/?",
    shell:
        "quarto render {input.qmd}"

## readme: Render readme
rule readme:
    input:
        qmd = 'README.qmd',
    output:
        md="README.md",
    shell:
        "quarto render {input.qmd}"

## dag: Generate DAG
rule dag:
    input:
        "Snakefile",
    output:
        png="dag.png",
    shell:
        "snakemake --rulegraph | dot -T png > {output.png}"

## coverage: Generate test coverage report with codecov
rule coverage:
    output:
        report="coverage.html",
    shell:
        """
        Rscript -e "covr::report(
            covr::package_coverage(quiet = FALSE),
            file = \'coverage.html\',
            browse = FALSE
        )"
        """

## all: Default
rule all:
    default_target: True
    input:
        rules.dag.output.png,
        rules.data_raw.input,
        rules.check.output.flag,
        rules.pkgdown.output.flag,
        "README.md",

## clean: Remove flags, manpages, data, and crosstab downloads
rule clean:
    shell:
        """
        rm -f .flags/* \
            *.html \
            man/*.Rd \
            data/*.rda \
            R/sysdata.rda 
        """

## help: Print help, do nothing
rule help:
    input:
        'Snakefile',
    shell:
        'sed -n "s/^## /* /p" {input}'
