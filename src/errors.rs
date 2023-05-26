use lazy_static::lazy_static;

lazy_static! {
    pub static ref ERR_INVALID_ARG_LABEL: String = "invalid_label".to_string();
    pub static ref ERR_INVALID_ARG_CODE: i64 = 1;
    pub static ref ERR_OVERFLOW_LABEL: String = "overflow_label".to_string();
    pub static ref ERR_OVERFLOW_CODE: i64 = 2;
    pub static ref ERR_MEMORY_LIMIT_LABEL: String = "memory_limit_label".to_string();
    pub static ref ERR_MEMORY_LIMIT_CODE: i64 = 3;
    pub static ref ERR_OUT_OF_BOUND_LABEL: String = "out_of_bound_label".to_string();
    pub static ref ERR_OUT_OF_BOUND_CODE: i64 = 4;
}
