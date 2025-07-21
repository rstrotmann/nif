# Problems Found in `testcd()` Function

## Summary
The comprehensive tests revealed several critical issues in the `testcd()` function that could cause runtime errors and unexpected behavior.

## Critical Issues

### 1. Multiple Domain Handling Bug
**Error**: `the condition has length > 1`
**Location**: Line 956 in `R/sdtm_class.R`
**Problem**: The function assumes each data frame has a single unique domain. When multiple domains exist in one data frame, `unique(toupper(x$DOMAIN))` returns a vector of length > 1, causing the `if` condition to fail.

**Code causing the issue**:
```r
domain <- unique(toupper(x$DOMAIN))
if (length(domain) == 0) {
  warning("DOMAIN column is empty")
  return(acc)
}

testcd_col <- paste0(domain, "TESTCD")  # domain is a vector, not scalar
out <- NULL
if(testcd_col %in% names(x)){  # This fails when domain has length > 1
```

### 2. Data Type Incompatibility
**Error**: `Can't combine <character> and <logical>` or `<character> and <double>`
**Problem**: The function doesn't handle different data types in TESTCD columns consistently. When combining results from different domains, if one has character TESTCD values and another has logical/numeric, `bind_rows()` fails.

**Affected scenarios**:
- Logical TESTCD values (TRUE/FALSE)
- Numeric TESTCD values (123, 456)
- Mixed data types across domains

### 3. NULL Value Handling
**Error**: `arguments imply differing number of rows: 1, 0`
**Problem**: The function doesn't handle NULL values in TESTCD columns properly. When trying to create a data frame with NULL values, it causes row count mismatches.

### 4. Case Sensitivity Issues
**Problem**: Domain parameter case sensitivity is not handled correctly. When using uppercase domain names like "DM", the function doesn't find the domain because it's looking for lowercase names in the domain list.

**Issue**: The function uses `tolower()` for domain comparison but `toupper()` for domain extraction, creating inconsistency.

### 5. Validation Function Issue
**Error**: `argument "param_name" is missing, with no default`
**Problem**: The `validate_char_param()` function is not being called correctly, missing the parameter name.

**Current code**:
```r
validate_char_param(domain, allow_null = TRUE, allow_multiple = TRUE)
```

**Should be**:
```r
validate_char_param(domain, param_name = "domain", allow_null = TRUE, allow_multiple = TRUE)
```

## Minor Issues

### 6. Empty DOMAIN Column Handling
**Warning**: "DOMAIN column is empty"
**Problem**: The function warns but doesn't handle empty DOMAIN columns gracefully.

### 7. Missing DOMAIN Column Handling
**Warning**: "Domain data frame missing DOMAIN column"
**Problem**: The function warns but continues processing, which may lead to unexpected results.

## Recommended Fixes

1. **Fix multiple domain handling**: Process each domain separately when multiple domains exist in one data frame
2. **Standardize data types**: Convert all TESTCD values to character type before processing
3. **Improve NULL handling**: Filter out NULL values or handle them explicitly
4. **Fix case sensitivity**: Use consistent case handling throughout the function
5. **Fix validation call**: Add missing parameter name to validation function
6. **Improve error handling**: Add more robust error handling for edge cases

## Test Coverage
The new tests cover:
- NA values in TESTCD columns
- Multiple domains in single data frame
- Domain parameter handling
- Case sensitivity
- Empty data frames
- Duplicate TESTCD values
- Invalid input types
- Domain parameter validation
- Inconsistent DOMAIN values
- All NA TESTCD values
- Empty TESTCD values
- Whitespace in TESTCD values
- Special characters in TESTCD values
- Numeric TESTCD values
- Very long TESTCD values
- Unicode characters in TESTCD values
- Missing TESTCD columns
- NULL values in TESTCD columns
- Factor TESTCD values
- Logical TESTCD values
- Actual numeric TESTCD values
- Complex TESTCD values

## Impact
These issues could cause:
- Runtime errors in production code
- Inconsistent results depending on data structure
- Silent failures with unexpected behavior
- Poor user experience with unclear error messages 