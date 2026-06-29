# Norwegian workdays and holidays by date

Norwegian workdays and holidays by date

## Usage

``` r
nor_workdays_by_date
```

## Format

- date:

  Date.

- day_of_week:

  Integer. 1 = Monday, 7 = Sunday

- mon_to_fri:

  Integer. 1 between Monday and Friday, 0 between Saturday and Sunday

- sat_to_sun:

  Integer. 1 between Saturday and Sunday, 0 between Monday and Friday

- public_holiday:

  Integer. 1 if public holiday (helligdag), 0 if not public holiday

- freeday:

  Integer. 1 if public holiday (helligdag) or sat_to_sun==1, 0 otherwise

- workday:

  Integer. 1 if freeday==0, 0 if freeday==1
