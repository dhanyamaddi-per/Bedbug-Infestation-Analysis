select * from final.bedbug_reporting;

select Borough,count(Borough) from final.bedbug_reporting  group by Borough;

SELECT `Council District`,count(`Council District`)
FROM final.bedbug_reporting where "2010 Census Tract">=28 group by `Council District`;

SHOW COLUMNS FROM final.bedbug_reporting;
select min(`Community Board`),max(`Community Board`) from final.bedbug_reporting;
select avg(`# of Dwelling Units`),std(`# of Dwelling Units`) from final.bedbug_reporting;

select `Infested Dwelling Unit Count`,count(`Infested Dwelling Unit Count`) from final.bedbug_reporting where borough="Queens" group by `Infested Dwelling Unit Count`;
select min(`Re-infested  Dwelling Unit Count`),max(`Re-infested  Dwelling Unit Count`),avg(`Re-infested  Dwelling Unit Count`),std(`Re-infested  Dwelling Unit Count`) 
from final.bedbug_reporting;

select `NTA`,Borough from final.bedbug_reporting where `# of Dwelling Units`>600;

SELECT DISTINCT `Filing Period Start Date`
FROM final.bedbug_reporting
ORDER BY `Filing Period Start Date` DESC
LIMIT 10;

select distinct `# of Dwelling Units` from final.bedbug_reporting where `Filing Period Start Date`>="01-01-2020";
select distinct Borough, ( `Filing Period Start Date`) from final.bedbug_reporting 
where `Filing Period Start Date`>="09-01-2016" 
order  by Borough;

select distinct Borough, ( `Filling Period End Date`) from final.bedbug_reporting 
where `Filling Period End Date`>="09-01-2017" 
order  by Borough;

select distinct Borough, ( `Filing Period Start Date`),count(Borough) from final.bedbug_reporting 
where `Filing Period Start Date`>="09-01-2017" 
group by Borough,`Filing Period Start Date`
order by `Filing Period Start Date`;

select distinct Borough, ( `Filling Period End Date`),count(Borough) from final.bedbug_reporting 
where `Filling Period End Date`>="09-01-2017" 
group by Borough,`Filling Period End Date`
order by `Filling Period End Date`;






