use crate::lint::rules::tests::common::assert_snippet;
use crate::lint::rules::instantiate_rules;
use crate::lint::LintCfg;

// Put whitespace (space or newline):
//  SP.reserved around reserved words, such as if, else, default,
//  size, const and in, except when a reserved word is used as an identifier
//  (e.g., local uint8 *data;)
static SP_RESERVED: &str = "
method this_is_some_method() {
local int this_some_integer = 0x666;
if(this_some_integer == 0x666)
    return;
}
";

//  SP.braces around braces ({ and })
static SP_BRACES: &str = "
method this_is_some_method() {return 0;}

method this_is_empty_method() { }

bank pcie_config {register command {field mem {
    method pcie_write(uint64 value) {
        if (value != 0) {value = value + 1;}
        default(value);
        map_memory_alt();
    }
}
}
}
";
#[test]
fn style_check_sp_braces() {
    let mut cfg = LintCfg::default();
    let mut rules = instantiate_rules(&cfg);
    assert_snippet(SP_BRACES, 6, &rules);
    // Test rule disable
    cfg.sp_brace = None;
    rules = instantiate_rules(&cfg);
    assert_snippet(SP_BRACES, 0, &rules);

}

static SP_BRACES_02: &str = "
typedef struct {uint16 idx;} hqm_cq_list_release_ctx_t;

typedef layout \"little-endian\" {bitfields 1 {uint1 cq @ [0:0];} byte;} q_t;
";
#[test]
fn style_check_sp_braces_02() {
    let mut cfg = LintCfg::default();
    let mut rules = instantiate_rules(&cfg);
    assert_snippet(SP_BRACES_02, 6, &rules);
    // Test rule disable
    cfg.sp_brace = None;
    rules = instantiate_rules(&cfg);
    assert_snippet(SP_BRACES_02, 0, &rules);

}

//  SP.binop around binary operators except the dereferencing operators dot
//  (a.b) and arrow (a->b)
static SP_BINOP: &str = "
method this_is_some_method() {
local int this_some_integer = 5+6;
if (this_some_integer == 0x666)
    this_some_integer = this.val;
}
";

//  SP.ternary around ? and : in the ternary ?: operator
static SP_TERNARY: &str = "
method this_is_some_method(bool flag) {
local int this_some_integer = (flag?5:7));
}
";

//  SP.punct after but not before colon, semicolon and comma
static SP_PUNCT: &str = "
method this_is_some_method(bool flag ,int8 var) {
    local int this_some_integer = 0x666 ;
    if(this_some_integer == 0x666)
        return;
    some_func(arg1 ,arg2 ,arg3 ,arg4);
}
";
#[test]
fn style_check_sp_punct_rule() {
    let mut cfg = LintCfg::default();
    let mut rules = instantiate_rules(&cfg);
    assert_snippet(SP_PUNCT, 9, &rules);
    // Test rule disable
    cfg.sp_punct = None;
    rules = instantiate_rules(&cfg);
    assert_snippet(SP_PUNCT, 0, &rules);
}

//  SP.ptrdecl between a type and the * marking a pointer
static SP_PTRDECL: &str = "
method this_is_some_method(conf_object_t* dummy_obj) {
if(!dummy_obj)
    return;
}
";

//  SP.comment around the comment delimiters //, /* and **/
static SP_COMMENT: &str = "
/*Function
documentation*/
method this_is_some_method(conf_object_t *dummy_obj) {
if(!dummy_obj)//Not null
    return;
}
";

// There should be no space:
//  NSP.funpar between a function/method name and its opening parenthesis
static NSP_FUNPAR: &str = "
method this_is_some_method (conf_object_t *dummy_obj) {
    if(!dummy_obj)
        other_method_called ();
}
";
#[test]
fn style_check_nsp_funpar() {
    let mut cfg = LintCfg::default();
    let mut rules = instantiate_rules(&cfg);
    assert_snippet(NSP_FUNPAR, 2, &rules);
    // Test rule disable
    cfg.nsp_funpar = None;
    rules = instantiate_rules(&cfg);
    assert_snippet(NSP_FUNPAR, 0, &rules);
}

//  NSP.inparen immediately inside parentheses or brackets
static NSP_INPAREN: &str = "
method this_is_some_method( conf_object_t *dummy_obj ) {
    if( !dummy_obj[ 0 ] )
        return;
}
";
#[test]
fn style_check_nsp_inparen() {
    let mut cfg = LintCfg::default();
    let mut rules = instantiate_rules(&cfg);
    assert_snippet(NSP_INPAREN, 6, &rules);
    // Test rule disable
    cfg.nsp_inparen = None;
    rules = instantiate_rules(&cfg);
    assert_snippet(NSP_INPAREN, 0, &rules);
}

//  NSP.unary between a unary operator and its operand
static NSP_UNARY: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    if(! dummy_obj)
        return;
    local uint64 p = & dummy_obj;
    p ++;
    -- p;
    local int64 neg = - 1;
}
";
#[test]
fn style_check_nsp_unary() {
    let mut cfg = LintCfg::default();
    let mut rules = instantiate_rules(&cfg);
    assert_snippet(NSP_UNARY, 5, &rules);
    // Test rule disable
    cfg.nsp_unary = None;
    rules = instantiate_rules(&cfg);
    assert_snippet(NSP_UNARY, 0, &rules);
}

//  NSP.ptrdecl after the * marking a pointer in a declaration
static NSP_PTRDECL: &str = "
method this_is_some_method(conf_object_t * dummy_obj) {
if(!dummy_obj)
    return;
}
";

//  Adding trailing whitespace removal to spacing rules:
//  no whitespaces should be left at the end of a line between the last token
//  and the newline \n
static NSP_TRAILING: &str = "
method this_is_some_method(int64 num) {
    local int this_some_integer = 0x666;           
    if (this_some_integer == 0x666)       
        return;  
}   
";
#[test]
fn style_check_nsp_trailing() {
    let mut cfg = LintCfg::default();
    let mut rules = instantiate_rules(&cfg);
    assert_snippet(NSP_TRAILING, 4, &rules);
    // Test rule disable
    cfg.nsp_trailing = None;
    rules = instantiate_rules(&cfg);
    assert_snippet(NSP_TRAILING, 0, &rules);
}
