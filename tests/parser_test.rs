use nutdb::parser::Parser;

macro_rules! test_sql_files {
    ($($fn_name:ident, $filename:expr;)+) => {
        $(
        #[test]
        fn $fn_name() {
            let sql = include_str!(concat!("sql/", $filename));
            let res = Parser::parse(sql);
            println!("{:#?}", res);
            if res.is_err() {
                eprintln!("{:#?}", res);
            }
            assert!(res.is_ok());
        }
        )+
    }
}

test_sql_files!(
    parse_sql_file_1, "1.sql";
    parse_sql_file_2, "2.sql";
    parse_sql_file_3, "3.sql";
    parse_sql_file_4, "4.sql";
    parse_sql_file_5, "5.sql";
    parse_sql_file_6, "6.sql";
    parse_sql_file_7, "7.sql";
    parse_sql_file_8, "8.sql";
    parse_sql_file_9, "9.sql";
    parse_sql_file_10, "10.sql";
    parse_sql_file_11, "11.sql";
    parse_sql_file_12, "12.sql";
    parse_sql_file_13, "13.sql";
    parse_sql_file_14, "14.sql";
);
