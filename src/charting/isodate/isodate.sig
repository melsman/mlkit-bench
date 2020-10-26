(* Copyright 2015, Martin Elsman, MIT-license *)

signature ISO_DATE = sig
  eqtype t
  val lt           : t * t -> bool
  val lte          : t * t -> bool
  val compare      : t * t -> order
  val min          : t * t -> t
  val max          : t * t -> t
  val fromString   : string -> t option
  val toString     : t -> string
  val toStringWithSep : string -> t -> string
  val next         : t -> t
  val nextBusiness : t -> t
  val prev         : t -> t
  val date         : {year:int, month:int, day:int} -> t
  val toYMD        : t -> {year:int, month:int, day:int}
  val year         : t -> int
  val month        : t -> int
  val day          : t -> int

  val minus        : t * t -> int
  val epoch        : t -> real
  val fromEpoch    : real -> t

  val weekday      : t -> int  (* value in range [0;6] - 0 is Sunday *)

  val toDate       : t -> Date.date
  val fromDate     : Date.date -> t

  val fromMonth    : Date.month -> int
  val toMonth      : int -> Date.month option

  val firstInYear  : t -> t
  val firstInMonth : t -> t
  val minus1Year   : t -> t
  val minus1Week   : t -> t
  val minusNMonths : t -> int -> t

  (* zipTimes takes two timeseries `(date * real) list` and zips them according
   * to the dates. If both series do not have an entry on a specific date, that
   * date is left out of the result entirely. *)
  val zipTimeseries : (t * real) list * (t * real) list -> (t * (real * real)) list

  (* zipTimeseriesPartial is like zipTimeseries, except that if one of the
   * series do not have an entry on a specific date, the date is still included
   * but with a NONE value for the missing data point. *)
  val zipTimeseriesPartial : (t * real) list * (t * real) list ->
                             (t * (real option * real option)) list

  val fromExcel1900 : int -> t
  val fromExcel1904 : int -> t

  type monthday = {month:int, day:int}

  val dateFilter : t -> t -> ('a -> t) -> 'a list -> 'a list

  structure MonthDay : sig
    val fromString : string -> monthday option
    val toString   : monthday -> string
    val prevDate   : monthday list -> t -> t
    val numPrev    : monthday list -> t -> int
    val numNext    : monthday list -> t -> int
  end

end
