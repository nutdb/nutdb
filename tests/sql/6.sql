with c_orders as (
    select
        c_custkey,
        count(o_orderkey) as c_count
    from
        customer left outer join orders on
                    c_custkey = o_custkey
                and o_comment not like '%special%requests%'
    group by
        c_custkey
)
select
	c_count,
	count(*) as custdist
from
    c_orders
where
    total_revenue = (
        select
            max(total_revenue)
        from
            revenue0
    )
group by
	c_count
order by
	custdist desc,
	c_count desc;
