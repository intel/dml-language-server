mod nsp_funpar;
mod nsp_inparen;
mod nsp_trailing;
mod nsp_unary;
mod sp_braces;
mod sp_punct;

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

//  SP.binop around binary operators except the dereferencing operators dot
//  (a.b) and arrow (a->b)
#[allow(dead_code)]
static SP_BINOP: &str = "
method this_is_some_method() {
local int this_some_integer = 5+6;
if (this_some_integer == 0x666)
    this_some_integer = this.val;
}
";

//  SP.ternary around ? and : in the ternary ?: operator
#[allow(dead_code)]
static SP_TERNARY: &str = "
method this_is_some_method(bool flag) {
local int this_some_integer = (flag?5:7));
}
";

//  SP.ptrdecl between a type and the * marking a pointer
#[allow(dead_code)]
static SP_PTRDECL: &str = "
method this_is_some_method(conf_object_t* dummy_obj) {
if(!dummy_obj)
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

//  NSP.ptrdecl after the * marking a pointer in a declaration
#[allow(dead_code)]
static NSP_PTRDECL: &str = "
method this_is_some_method(conf_object_t * dummy_obj) {
if(!dummy_obj)
    return;
}
";
