use lazy_static::lazy_static;

lazy_static! {
    pub static ref ERR_INVALID_ARG_LABEL: String = "invalid_label_0".to_string();
    pub static ref ERR_INVALID_ARG_CODE: i64 = 1;
    pub static ref ERR_OVERFLOW_LABEL: String = "overflow_label_1".to_string();
    pub static ref ERR_OVERFLOW_CODE: i64 = 2;
}
