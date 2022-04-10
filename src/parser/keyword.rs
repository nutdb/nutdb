/// ```plain
/// declare_keywords! {
///     keyword_name:ident => "keyword",
///     ...
/// }
/// ```
macro_rules! declare_keywords {
    ($($name:ident => $lit:literal),+) => {
        $(pub static $name: &'static str = $lit;)+
    };
}

declare_keywords! {
    // common
    BY => "by",
    AS => "as",
    ON => "on",
    FROM => "from",

    // set operation
    INTERSECT => "intersect",
    UNION => "union",
    ALL => "all",
    EXCEPT => "except",
    DISTINCT => "distinct",

    // select
    WITH => "with",
    SELECT => "select",
    JOIN => "join",
    WHERE => "where",
    GROUP => "group",
    HAVING => "having",
    ORDER => "order",
    LIMIT => "limit",
    OFFSET => "offset",
    USING => "using",
    TIES => "ties",

    // order direction
    ASC => "asc",
    DESC => "desc",

    // explain
    EXPLAIN => "explain",

    // insert
    INSERT => "insert",
    INTO => "into",
    VALUES => "values",

    // create
    CREATE => "create",
    PRIMARY => "primary",
    KEY => "key",
    COMMENT => "comment",
    UPDATE => "update",
    DEFAULT => "default",
    CHECK => "check",

    // describe
    DESCRIBE => "describe",

    // drop
    DROP => "drop",

    // alter
    ALTER => "alter",
    ADD => "add",
    RENAME => "rename",
    FIRST => "first",
    AFTER => "after",

    // truncate
    TRUNCATE => "truncate",

    // optimize
    OPTIMIZE => "optimize",

    // set
    SET => "set",

    // entity
    DATABASE => "database",
    TABLE => "table",
    VIEW => "view",
    COLUMN => "column",
    INDEX => "index",
    CONSTRAINT => "constraint",
    PARTITION => "partition",

    // null
    NULL => "null",

    // boolean
    TRUE => "true",
    FALSE => "false",

    // logical operator
    AND => "and",
    OR => "or",
    XOR => "xor",
    NOT => "not",
    IN => "in",
    EXISTS => "exists",

    // flow
    IF => "if",
    CASE => "case",
    WHEN => "when",
    THEN => "then",
    ELSE => "else",
    END => "end",

    // cmp
    IS => "is",
    BETWEEN => "between",
    LIKE => "like",
    ILIKE => "ilike",

    // interval
    INTERVAL => "interval",
    SECOND => "second",
    MINUTE => "minute",
    HOUR => "hour",
    DAY => "day",
    MONTH => "month",
    YEAR => "year",

    // data types
    INT8 => "int8", INT16 => "int16", INT32 => "int32", INT64 => "int64", INT128 => "int128",
    UINT8 => "uint8", UINT16 => "uint16", UINT32 => "uint32", UINT64 => "uint64", UINT128 => "uint128",
    SERIAL32 => "serial32", SERIAL64 => "serial64", SERIAL128 => "serial128",
    USERIAL32 => "userial32", USERIAL64 => "userial64", USERIAL128 => "userial128",
    DECIMAL32 => "decimal32", DECIMAL64 => "decimal64",
    FLOAT32 => "float32", FLOAT64 => "float64",
    BOOLEAN => "boolean",
    CHARS => "chars", STRING => "string",
    UUID => "uuid",
    DATE => "date", DATETIME => "datetime",
    ARRAY => "array", ENUM => "enum", TUPLE => "tuple", MAP => "map",
    DICTIONARY => "dictionary", NULLABLE => "nullable",

    // join type
    INNER => "inner", OUTER => "outer",
    LEFT => "left", RIGHT => "right",
    FULL => "full", SEMI => "semi", ANTI => "anti"
}
