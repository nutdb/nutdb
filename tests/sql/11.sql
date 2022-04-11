CREATE TABLE IF NOT EXISTS uk_price_paid
(
    price UInt32,
    date Date,
    postcode1 Dictionary(String),
    postcode2 Dictionary(String),
    type Enum('terraced' = 1, 'semi-detached' = 2, 'detached' = 3, 'flat' = 4, 'other' = 0),
    is_new UInt8,
    duration Enum('freehold' = 1, 'leasehold' = 2, 'unknown' = 0),
    addr1 String,
    addr2 String,
    street Dictionary(String),
    locality Dictionary(String),
    town Dictionary(String),
    district Dictionary(String),
    county Dictionary(String),
    category UInt8,
    INDEX idx_price minmax(price),
    CONSTRAINT c_is_new CHECK is_new < 2
)
ORDER BY (postcode1, postcode2, addr1, addr2)