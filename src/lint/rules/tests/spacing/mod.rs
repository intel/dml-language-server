mod nsp_funpar;
mod nsp_inparen;
mod nsp_ptrdecl;
mod nsp_trailing;
mod nsp_unary;
mod sp_braces;
mod sp_ptrdecl;
mod sp_punct;
mod sp_binop;
mod sp_ternary;

// Put whitespace (space or newline):
//  SP.reserved around reserved words, such as if, else, default,
//  size, const and in, except when a reserved word is used as an identifier
//  (e.g., local uint8 *data;)
#[allow(dead_code)]
static SP_RESERVED: &str = "
method this_is_some_method() {
local int this_some_integer = 0x666;
if(this_some_integer == 0x666)
    return;
}
";

//  SP.comment around the comment delimiters //, /* and **/
#[allow(dead_code)]
static SP_COMMENT: &str = "
/*Function
documentation*/
method this_is_some_method(conf_object_t *dummy_obj) {
if(!dummy_obj)//Not null
    return;
}
";

