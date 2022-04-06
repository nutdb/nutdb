use crate::parser::ast::{ColumnDefinition, ConstraintDefinition, IndexDefinition, ScalarValue};

/// alter table entities or table properties
#[derive(Debug, Clone)]
pub enum Alter {
    AddColumn {
        table_name: String,
        def: ColumnDefinition,
        if_non_exists: bool,
        position: EntityPosition,
    },
    AddConstraint {
        table_name: String,
        def: ConstraintDefinition,
        if_non_exists: bool,
        position: EntityPosition,
    },
    AddIndex {
        table_name: String,
        def: IndexDefinition,
        if_non_exists: bool,
        position: EntityPosition,
    },
    RenameColumn {
        table_name: String,
        old_name: String,
        new_name: String,
    },
    RenameConstraint {
        table_name: String,
        old_name: String,
        new_name: String,
    },
    RenameIndex {
        table_name: String,
        old_name: String,
        new_name: String,
    },
    RenameTable {
        old_name: String,
        new_name: String,
    },
    DropColumn {
        table_name: String,
        name: String,
        if_exists: bool,
    },
    DropConstraint {
        table_name: String,
        name: String,
        if_exists: bool,
    },
    DropIndex {
        table_name: String,
        name: String,
        if_exists: bool,
    },
    DropPartition {
        table_name: String,
        /// partition has no name but a ScalarValue
        partition_key: ScalarValue,
        if_exists: bool,
    },
}

#[derive(Debug, Clone)]
pub enum EntityPosition {
    First,
    After(String),
    Last,
}
