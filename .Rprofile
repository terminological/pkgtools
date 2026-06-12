# Patch jquerylib::expand_version for missing libjs-jquery system package
# Falls back to bundled jQuery 3.6.1 when system package is not installed
patched_expand_version <- function(major_version) {
    actual_version <- system("dpkg -s libjs-jquery 2>/dev/null | grep '^Version' | sed -e 's/Version:[[:space:]]//g' | cut -d'.' -f1", intern = TRUE)
    full_version <- system("dpkg -s libjs-jquery 2>/dev/null | grep '^Version' | sed -e 's/Version:[[:space:]]//g' | cut -d'+' -f1 | cut -d'-' -f1", intern = TRUE)
    # Fall back to bundled jQuery when system package is not installed
    if (length(actual_version) == 0) {
        return(paste0(major_version, ".6.1"))
    }
    if (major_version != actual_version) return("")
    return(full_version)
}
env <- asNamespace("jquerylib")
if (exists("expand_version", envir = env)) {
    unlockBinding("expand_version", env)
    env$expand_version <- patched_expand_version
    lockBinding("expand_version", env)
}
