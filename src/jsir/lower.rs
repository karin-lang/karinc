use crate::hir;
use crate::parser::ast;
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct JsirLoweringLog {}

pub struct JsirLowering {
    pub logs: Vec<JsirLoweringLog>,
}

impl JsirLowering {
    pub fn new() -> JsirLowering {
        JsirLowering { logs: Vec::new() }
    }

    pub fn lower(mut self, hir: &hir::Hir) -> Jsir {
        let mut items = Vec::new();

        for (each_symbol, each_entity) in &hir.global_entities {
            let new_item = self.lower_global_entity(each_symbol, each_entity);
            items.push(new_item);
        }

        Jsir { items }
    }

    pub fn lower_global_entity(&mut self, symbol: &ast::GlobalSymbol, entity: &ast::GlobalEntity) -> Item {
        match entity {
            ast::GlobalEntity::FnDecl(decl) => {
                let args = self.lower_formal_args(&decl.args);
                let stmts = self.lower_body(&decl.body);
                let jsir_decl = FnDecl { symbol: symbol.clone(), args, stmts };
                Item::FnDecl(jsir_decl)
            },
        }
    }

    pub fn lower_formal_args(&mut self, args: &Vec<ast::FormalArg>) -> Vec<FormalArg> {
        args.iter().map(|each_arg| FormalArg { id: each_arg.id.clone() }).collect()
    }

    pub fn lower_body(&mut self, exprs: &Vec<ast::Expr>) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        for each_expr in exprs {
            if let Some(new_stmt) = self.lower_expr(each_expr) {
                stmts.push(new_stmt);
            }
        }
        stmts
    }

    pub fn lower_expr(&mut self, symbol_table: ast::LocalSymbolTable, expr: &ast::Expr) -> Option<Stmt> {
        match &expr.kind {
            ast::ExprKind::Id(id, symbol) => return None,
            ast::ExprKind::LocalEntity(symbol) => {
                let entity = symbol_table.
            },
        }
    }
}
