use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeCheckLog {
    TypeUnmatch,
}

pub struct TypeCheck {
    logs: Vec<TypeCheckLog>,
}

impl TypeCheck {
    pub fn new() -> TypeCheck {
        TypeCheck { logs: Vec::new() }
    }

    pub fn infer_expr_type(&mut self, locals: &Vec<Local>, expr: &Expr) -> DerivedType {
        match &expr.kind {
            ExprKind::PathRef(r#type) => r#type.clone(),
            ExprKind::LocalDecl(local_id) => match locals.get(usize::from(local_id)) {
                Some(_) => DerivedType::new(DerivedTypeKind::Void),
                None => DerivedType::new(DerivedTypeKind::Undefined),
            },
            ExprKind::LocalRef(local_id) => match locals.get(usize::from(local_id)) {
                Some(local) => match local {
                    Local::FormalArg(arg) => arg.r#type.clone(),
                    Local::VarDecl(decl) => match &decl.r#type {
                        Type::Derived(r#type) => r#type.clone(),
                        Type::Infer(_) => ,
                    },
                    Local::VarInit(_) => ,
                    Local::Undefined => DerivedType::new(DerivedTypeKind::Undefined),
                },
                None => DerivedType::new(DerivedTypeKind::Undefined),
            },
        }
    }

    pub fn check(mut self, tir: &Tir) -> Vec<TypeCheckLog> {
        for (_, each_body) in &tir.bodies {
            self.check_body(each_body);
        }
        self.logs
    }

    pub fn check_body(&mut self, body: &Body) {
        for each_expr in &body.exprs {
            self.check_expr(&body.locals, each_expr);
        }
    }

    pub fn check_expr(&mut self, locals: &Vec<Local>, expr: &Expr) {
        match &expr.kind {
            ExprKind::PathRef(_) => unimplemented!(),
            ExprKind::LocalDecl(local_id) => match locals.get(local_id.into()) {
                Some(local) => self.check_local_decl(locals, local),
                None => unimplemented!(),
            },
            ExprKind::LocalRef(_) => unimplemented!(),
        }
    }

    pub fn check_local_decl(&mut self, locals: &Vec<Local>, local: &Local) {
        match local {
            Local::VarInit(init) => {
                let r#type = self.infer_expr_type(locals, &init.init);
            },
            _ => unimplemented!(),
        }
    }
}
