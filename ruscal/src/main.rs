use std::{collections::HashMap, io::Read};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
	alpha1, alphanumeric1, char, multispace0, multispace1
    },
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
    Finish, IResult, Parser
};

fn main() {
    let mut buf = String::new();
    
    if !std::io::stdin().read_to_string(&mut buf).is_ok() {
	panic!("Failed to read from stdin");
    }

    let parsed_statements = match statements_finish(&buf) {
	Ok(parsed_statements) => parsed_statements,
	Err(e) => {
	    eprintln!("Parser error: {:?}", e);
	    return;
	}
    };

    let mut frame = StackFrame::new();

    eval_stmts(&parsed_statements, &mut frame);
}

enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn),
}

impl<'src> FnDef<'src> {
    fn call(&self, args: &[f64], frame: &StackFrame) -> f64 {
	match self {
	    Self::User(code) => {
		let mut new_frame = StackFrame::push_stack(frame);
		new_frame.vars = args
		    .iter()
		    .zip(code.args.iter())
		    .map(|(arg, name)| (name.to_string(), *arg))
		    .collect();
		eval_stmts(&code.stmts, &mut new_frame)
	    }
	    Self::Native(code) => (code.code)(args)
	}
    }
}

struct UserFn<'src> {
    args: Vec<&'src str>,
    stmts: Statements<'src>,
}

struct NativeFn {
    code: Box<dyn Fn(&[f64]) -> f64>,
}

type Variables = HashMap<String, f64>;
type Functions<'src> = HashMap<String, FnDef<'src>>;

struct StackFrame<'src> {
    vars: Variables,
    funcs: Functions<'src>,
    uplevel: Option<&'src StackFrame<'src>>,
}

impl<'src> StackFrame<'src> {
    fn new() -> Self {
	let mut funcs = Functions::new();
	funcs.insert("sqrt".to_string(), unary_fn(f64::sqrt));
	funcs.insert("sin".to_string(), unary_fn(f64::sin));
	funcs.insert("cos".to_string(), unary_fn(f64::cos));
	funcs.insert("tan".to_string(), unary_fn(f64::tan));
	funcs.insert("asin".to_string(), unary_fn(f64::asin));
	funcs.insert("acos".to_string(), unary_fn(f64::acos));
	funcs.insert("atan".to_string(), unary_fn(f64::atan));
	funcs.insert("atan2".to_string(), binary_fn(f64::atan2));
	funcs.insert("pow".to_string(), binary_fn(f64::powf));
	funcs.insert("exp".to_string(), unary_fn(f64::exp));
	funcs.insert("log".to_string(), binary_fn(f64::log));
	funcs.insert("log10".to_string(), unary_fn(f64::log10));
	funcs.insert("print".to_string(), unary_fn(print));
	Self {
	    vars: Variables::new(),
	    funcs,
	    uplevel: None,
	}
    }

    fn get_fn(&self, name: &str) -> Option<&FnDef<'src>> {
	let mut next_frame = Some(self);
	while let Some(frame) = next_frame {
	    if let Some(func) = frame.funcs.get(name) {
		return Some(func);
	    }
	    next_frame = frame.uplevel;
	}
	None
    }

    fn push_stack(uplevel: &'src Self) -> Self {
	Self {
	    vars: HashMap::new(),
	    funcs: HashMap::new(),
	    uplevel: Some(uplevel),
	}
    }
}

fn print(arg: f64) -> f64 {
    println!("print: {arg}");
    0.
}

fn eval_stmts<'src>(stmts: &[Statement<'src>], frame: &mut StackFrame<'src>) -> f64 {
    let mut last_result = 0.;
    
    for statement in stmts {
	match statement {
	    Statement::Expression(expr) => {
		last_result = eval(expr, frame);
	    }
	    Statement::VarDef(name, expr) => {
		let value = eval(expr, frame);
		frame.vars.insert(name.to_string(), value);
	    }
	    Statement::VarAssign(name, expr) => {
		if !frame.vars.contains_key(*name) {
		    panic!("Variable {} not defined", name);
		}
		let value = eval(expr, frame);
		frame.vars.insert(name.to_string(), value);
	    }
	    Statement::For {
		loop_var,
		start,
		end,
		stmts,
	    } => {
		let start = eval(start, frame) as isize;
		let end = eval(end, frame) as isize;
		for i in start..end {
		    frame.vars.insert(loop_var.to_string(), i as f64);
		    eval_stmts(stmts, frame);
		}
	    }
	    Statement::FnDef { name, args, stmts } => {
		frame.funcs.insert(
		    name.to_string(),
		    FnDef::User(UserFn {
			args: args.clone(),
			stmts: stmts.clone(),
		    }),
		);
	    }
	}
    }
    last_result
}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    FnInvoke(&'src str, Vec<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    If(
	Box<Expression<'src>>,
	Box<Expression<'src>>,
	Option<Box<Expression<'src>>>,
    )
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    For {
	loop_var: &'src str,
	start: Expression<'src>,
	end: Expression<'src>,
	stmts: Statements<'src>,
    },
    FnDef {
	name: &'src str,
	args: Vec<&'src str>,
	stmts: Statements<'src>,
    }
}

type Statements<'a> = Vec<Statement<'a>>;

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
	code: Box::new(move |args| {
	    f(*args
              .into_iter()
              .next()
              .expect("function missing argument"))
	}),
    })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
	code: Box::new(move |args| {
	    let mut args = args.into_iter();
	    let lhs = args
		.next()
		.expect("function missing the first argument");
	    let rhs = args
		.next()
		.expect("function missing the second argument");
	    f(*lhs, *rhs)
	}),
    })
}

fn eval(expr: &Expression, frame: &StackFrame) -> f64 {
    use Expression::*;
    match expr {
	Ident("pi") => std::f64::consts::PI,
	Ident(id) => *frame.vars.get(*id).expect("Unknown variable"),
	NumLiteral(n) => *n,
	FnInvoke(name, args) => {
	    if let Some(func) = frame.get_fn(*name) {
		let args: Vec<_> = args.iter().map(|arg| eval(arg, frame)).collect();
		func.call(&args, frame)
	    } else {
		panic!("Unknown function {name:?}");
	    }
	},
	Add(lhs, rhs) => eval(lhs, frame) + eval(rhs, frame),
	Sub(lhs, rhs) => eval(lhs, frame) - eval(rhs, frame),
	Mul(lhs, rhs) => eval(lhs, frame) * eval(rhs, frame),
	Div(lhs, rhs) => eval(lhs, frame) / eval(rhs, frame),
	Gt(lhs, rhs) => {
	    if eval(lhs, frame) > eval(rhs, frame) {
		1.
	    } else {
		0.
	    }
	},
	Lt(lhs, rhs) => {
	    if eval(lhs, frame) < eval(rhs, frame) {
		1.
	    } else {
		0.
	    }
	},
	If(cond, t_case, f_case) => {
	    if eval(cond, frame) != 0. {
		eval(t_case, frame)
	    } else if let Some(f_case) = f_case {
		eval(f_case, frame)
	    } else {
		0.
	    }
	},
    }
}

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

fn factor(input: &str) -> IResult<&str, Expression> {
    alt((number, func_call, ident, parens))(input)
}

fn func_call(input: &str) -> IResult<&str, Expression> {
    let (r, ident) = space_delimited(identifier)(input)?;
    let (r, args) = space_delimited(delimited(
	tag("("),
	many0(delimited(
	    multispace0,
	    expr,
	    space_delimited(opt(tag(","))),
	)),
	tag(")"),
    ))(r)?;
    Ok((r, Expression::FnInvoke(ident, args)))
}

fn term(input: &str) -> IResult<&str, Expression> {
    let (i, init) = factor(input)?;

    fold_many0(
	pair(space_delimited(alt((char('*'), char('/')))), factor),
	move || init.clone(),
	|acc, (op, val): (char, Expression)| {
	    match op {
		'*' => Expression::Mul(Box::new(acc), Box::new(val)),
		'/' => Expression::Div(Box::new(acc), Box::new(val)),
		_ => panic!(
		    "Multiplicative expression should have '*' or '/' operator"
		),
	    }
	}
    )(i)
}

fn number(input: &str) -> IResult<&str, Expression> {
    let (r, v) = space_delimited(recognize_float)(input)?;

    Ok((
	r,
	Expression::NumLiteral(v.parse().map_err(|_| {
	    nom::Err::Error(nom::error::Error {
		input,
		code: nom::error::ErrorKind::Digit,
	    })
	})?),
    ))
}

fn ident(input: &str) -> IResult<&str, Expression> {
    let (r, res) = space_delimited(identifier)(input)?;
    Ok((r, Expression::Ident(res)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
	alt((alpha1, tag("_"))),
	many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn parens(input: &str) -> IResult<&str, Expression> {
    space_delimited(delimited(tag("("), expr, tag(")")))(input)
}

fn num_expr(input: &str) -> IResult<&str, Expression> {
    let (i, init) = term(input)?;

    fold_many0(
	pair(space_delimited(alt((char('+'), char('-')))), term),
	move || init.clone(),
	|acc, (op, val): (char, Expression)| {
	    match op {
		'+' => Expression::Add(Box::new(acc), Box::new(val)),
		'-' => Expression::Sub(Box::new(acc), Box::new(val)),
		_ => panic!(
		    "Additive expression should have '+' or '-' operator"
		),
	    }
	},
    )(i)
}

fn open_brace(input: &str) -> IResult<&str, ()> {
    let (input, _) = space_delimited(char('{'))(input)?;
    Ok((input, ()))
}

fn close_brace(input: &str) -> IResult<&str, ()> {
    let (input, _) = space_delimited(char('}'))(input)?;
    Ok((input, ()))
}

fn if_expr(input: &str) -> IResult<&str, Expression> {
    let (input, _) = space_delimited(tag("if"))(input)?;
    let (input, cond) = expr(input)?;
    let (input, t_case) = delimited(open_brace, expr, close_brace)(input)?;
    let (input, f_case) = opt(preceded(
	space_delimited(tag("else")), 
	delimited(open_brace, expr, close_brace)))
	(input)?;
    
    Ok((
	input,
	Expression::If(
	    Box::new(cond),
	    Box::new(t_case),
	    f_case.map(Box::new),
	),
    ))
}

fn cond_expr(input: &str) -> IResult<&str, Expression> {
    let (input, first) = num_expr(input)?;
    let (input, cond) = space_delimited(alt((char('<'), char('>'))))(input)?;
    let (input, second) = num_expr(input)?;
    
    Ok((
	input,
	match cond {
	    '<' => Expression::Lt(Box::new(first), Box::new(second)),
	    '>' => Expression::Gt(Box::new(first), Box::new(second)),
	    _ => unreachable!(),
	},
    ))
}



fn expr(input: &str) -> IResult<&str, Expression> {
    alt((if_expr, cond_expr, num_expr))(input)
}

fn statements(input: &str) -> IResult<&str, Statements> {
    let (input, stmts) = many0(statement)(input)?;
    let (input, _) = opt(char(';'))(input)?;
    Ok((input, stmts))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    alt((
	for_statement,
	fn_def_statement,
	terminated(
	    alt((var_def, var_assign, expr_statement)),
	    char(';'),
	),
    ))(input)
}

fn var_def(input: &str) -> IResult<&str, Statement> {
    let (input, _) = delimited(multispace0, tag("var"), multispace1)(input)?;
    let (input, name) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(char('='))(input)?;
    let (input, expr) = space_delimited(expr)(input)?;
    Ok((input, Statement::VarDef(name, expr)))
}

fn var_assign(input: &str) -> IResult<&str, Statement> {
    let (input, name) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(char('='))(input)?;
    let (input, expr) = space_delimited(expr)(input)?;
    Ok((input, Statement::VarAssign(name, expr)))
}

fn expr_statement(input: &str) -> IResult<&str, Statement> {
    let (input, res) = expr(input)?;
    Ok((input, Statement::Expression(res)))
}

fn for_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = space_delimited(tag("for"))(input)?;
    let (input, loop_var) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(tag("in"))(input)?;
    let (input, start) = space_delimited(expr)(input)?;
    let (input, _) = space_delimited(tag("to"))(input)?;
    let (input, end) = space_delimited(expr)(input)?;
    let (input, stmts) = delimited(open_brace, statements, close_brace)(input)?;

    Ok((
	input,
	Statement::For {
	    loop_var,
	    start,
	    end,
	    stmts
	},
    ))
}

fn fn_def_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = space_delimited(tag("fn"))(input)?;
    let (input, name) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(tag("("))(input)?;
    let (input, args) = separated_list0(char(','), space_delimited(identifier))(input)?;
    let (input, _) = space_delimited(tag(")"))(input)?;
    let (input, stmts) = delimited(open_brace, statements, close_brace)(input)?;
    Ok((input, Statement::FnDef { name, args, stmts }))
}

fn statements_finish(input: &str) -> Result<Statements, nom::error::Error<&str>> {
    let (_, res) = statements(input).finish()?;
    Ok(res)
}

