use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeCheckLog {
}

pub struct TypeCheck {
    logs: Vec<TypeCheckLog>,
}

impl TypeCheck {
    pub fn new() -> TypeCheck {
        TypeCheck { logs: Vec::new() }
    }

    pub fn check(mut self, tir: &Tir) -> Vec<TypeCheckLog> {
        for (_, each_body) in &tir.bodies {
            self.check_body(each_body);
        }
        self.logs
    }

    pub fn check_body(&mut self, body: &Body) {
        for each_expr in &body.exprs {
            self.check_expr(each_expr);
        }
    }

    pub fn check_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::PathRef(_) => (),
            ExprKind::LocalDecl(_) => (),
            ExprKind::LocalRef(_) => (),
        }
    }
}
