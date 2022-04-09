use derive_more::{Constructor, From};

use crate::parser::ast::{ColumnDefinition, ConstraintDefinition, IndexDefinition};
use crate::parser::Literal;

/// alter table entities or table properties
#[derive(Debug, Clone, From)]
pub enum Alter {
    AddColumn(AddColumn),
    AddConstraint(AddConstraint),
    AddIndex(AddIndex),
    RenameColumn(RenameColumn),
    RenameConstraint(RenameConstraint),
    RenameIndex(RenameIndex),
    RenameTable(RenameTable),
    DropColumn(DropColumn),
    DropConstraint(DropConstraint),
    DropIndex(DropIndex),
    DropPartition(DropPartition),
}

#[derive(Debug, Clone)]
pub enum EntityPosition {
    First,
    After(String),
    Last,
}

#[derive(Debug, Clone, Constructor)]
pub struct AddColumn {
    table_name: String,
    if_non_exists: bool,
    def: ColumnDefinition,
    position: EntityPosition,
}

#[derive(Debug, Clone, Constructor)]
pub struct AddConstraint {
    table_name: String,
    if_non_exists: bool,
    def: ConstraintDefinition,
    position: EntityPosition,
}

#[derive(Debug, Clone, Constructor)]
pub struct AddIndex {
    table_name: String,
    if_non_exists: bool,
    def: IndexDefinition,
    position: EntityPosition,
}

#[derive(Debug, Clone, Constructor)]
pub struct RenameColumn {
    table_name: String,
    old_name: String,
    new_name: String,
}

#[derive(Debug, Clone, Constructor)]
pub struct RenameConstraint {
    table_name: String,
    old_name: String,
    new_name: String,
}

#[derive(Debug, Clone, Constructor)]
pub struct RenameIndex {
    table_name: String,
    old_name: String,
    new_name: String,
}

#[derive(Debug, Clone, Constructor)]
pub struct RenameTable {
    old_name: String,
    new_name: String,
}

#[derive(Debug, Clone, Constructor)]
pub struct DropColumn {
    table_name: String,
    name: String,
    if_exists: bool,
}

#[derive(Debug, Clone, Constructor)]
pub struct DropConstraint {
    table_name: String,
    name: String,
    if_exists: bool,
}

#[derive(Debug, Clone, Constructor)]
pub struct DropIndex {
    table_name: String,
    name: String,
    if_exists: bool,
}

#[derive(Debug, Clone, Constructor)]
pub struct DropPartition {
    table_name: String,
    /// partition has no name but a ScalarValue
    partition_key: Literal,
    if_exists: bool,
}
