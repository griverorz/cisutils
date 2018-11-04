# CisUtils

`cisutils` is a small package to clean CIS data according to some rules contained in a JSON object. The goal is to simplify some of the tedious tasks of cleaning CIS data. It provides a few functions that can be chained in a pipeline. 

The normal use is illustrated here:

```r
dta <- input("cleaning-params.json", "study.sav")
dta <- dta %>%
    recode_missings %>%
    rename_variables %>%
    recode_values %>%
    known_recodes %>%
    retype %>%
    subselect
```

The `cleaning-params.json` is a JSON object that details how to clean the data. It is expected to include the fields "variables", "recodes", "missing", and "types". The "missing" field is defined through the original variable names, and the "recodes" and "types" is defined through the _renamed_ variable names. 

```json
{
    "variables": {
        "P30": "gender",
        "P31": "age",
        ...
        "P33": "religion"
    },
    "recodes": {
        "voted": {"1": "true",
                  "2": "false",
                  "3": "true",
                  "5": "false",
                  "6": "false"},
        ...
        "repeatedvote": {"1": "yes",
                         "2": "almost",
                         "3": "no"},
    },
    "missing": {
        "P30": null,
        ...
        "P33": 9
    },
    "types": {
        "ideology": "numeric",
        ...
        "ideolcs": "numeric"
    }
}

```

Note that all categories not included in "recodes" or "missing" for a given variable are coded as "other". 

The "types" field defines the type to which the variable will be encoded. All variables not listed here will be encoded as factor. Only two deviations from `factor` are expected: `numeric` and `ordered`.
