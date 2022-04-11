CREATE VIEW all_supplier_view
    UPDATE BY Summing
    ORDER BY supplyID
AS
SELECT supplyID, supplier
FROM SUPPLY1
WHERE sth = 1
UNION ALL
SELECT supplyID, supplier
FROM SUPPLY2
UNION ALL
SELECT supplyID, supplier
FROM SUPPLY3
UNION ALL
SELECT supplyID, supplier
FROM SUPPLY4;