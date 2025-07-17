mod nsp_funpar;
mod nsp_inparen;
mod nsp_ptrdecl;
mod nsp_trailing;
mod nsp_unary;
mod sp_braces;
mod sp_ptrdecl;
mod sp_punct;
mod sp_reserved;
mod sp_binop;
mod sp_ternary;


//  SP.comment around the comment delimiters //, /* and **/
#[allow(dead_code)]
static SP_COMMENT: &str = "
/*Function
documentation*/
method this_is_some_method(conf_object_t *dummy_obj) {
if (!dummy_obj)//Not null
    return;
}
";

