use derive_more::{Constructor, From};
use std::borrow::Cow;

use crate::parser::ast::{ColumnDefinition, ConstraintDefinition, IndexDefinition};

/// alter table entities or table properties
#[derive(Debug, Clone, From, Constructor)]
pub struct Alter<'a> {
    action: AlterAction<'a>,
    table_name: &'a str,
}

#[derive(Debug, Clone, From)]
pub enum AlterAction<'a> {
    Add {
        entity: AddableEntity<'a>,
        if_not_exists: bool,
        position: EntityPosition<'a>,
    },
    Rename {
        entity: RenamableEntity<'a>,
        new_name: &'a str,
    },
    Drop {
        entity: DroppableEntity<'a>,
        if_exists: bool,
    },
}

#[derive(Debug, Clone)]
pub enum EntityPosition<'a> {
    First,
    After(&'a str),
    Last,
}

#[derive(Debug, Clone)]
pub enum RenamableEntity<'a> {
    Column(&'a str),
    Constraint(&'a str),
    Index(&'a str),
    Table,
}

#[derive(Debug, Clone)]
pub enum AddableEntity<'a> {
    Column(ColumnDefinition<'a>),
    Constraint(ConstraintDefinition<'a>),
    Index(IndexDefinition<'a>),
}

#[derive(Debug, Clone)]
pub enum DroppableEntity<'a> {
    Column(&'a str),
    Constraint(&'a str),
    Index(&'a str),
    Partition(Cow<'a, str>),
}
