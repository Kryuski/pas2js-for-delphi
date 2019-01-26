{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit RTLConsts;

interface

const
  SArgumentMissing              = 'Missing argument in format "%s"';
  SInvalidFormat                = 'Invalid format specifier : "%s"';
  SInvalidArgIndex              = 'Invalid argument index in format: "%s"';
  SListCapacityError            = 'List capacity (%s) exceeded.';
  SListCountError               = 'List count (%s) out of bounds.';
  SListIndexError               = 'List index (%s) out of bounds';
  SSortedListError              = 'Operation not allowed on sorted list';
  SDuplicateString              = 'String list does not allow duplicates';  
  SErrFindNeedsSortedList       = 'Cannot use find on unsorted list';

  SInvalidName                  = 'Invalid component name: "%s"';
  SInvalidBoolean               = '"%s" is not a valid boolean.';
  SDuplicateName                = 'Duplicate component name: "%s"';

  SErrInvalidDate               = 'Invalid date: "%s"';
  SErrInvalidTimeFormat         = 'Invalid time format: "%s"';
  SInvalidDateFormat            = 'Invalid date format: "%s"';

  SCantReadPropertyS            = 'Cannot read property "%s"';
  SCantWritePropertyS           = 'Cannot write property "%s"';
  SErrPropertyNotFound          = 'Unknown property: "%s"';
  SIndexedPropertyNeedsParams   = 'Indexed property "%s" needs parameters';

  SErrInvalidInteger            = 'Invalid integer value: "%s"';
  SErrInvalidFloat              = 'Invalid floating-point value: "%s"';
  SInvalidDateTime              = 'Invalid date-time value: %s';
  SInvalidCurrency              = 'Invalid currency value: %s';
  SErrInvalidDayOfWeek          = '%d is not a valid day of the week';
  SErrInvalidTimeStamp          = 'Invalid date/timestamp : "%s"';
  SErrInvalidDateWeek           = '%d %d %d is not a valid dateweek';
  SErrInvalidDayOfYear          = 'Year %d does not have a day number %d';
  SErrInvalidDateMonthWeek      = 'Year %d, month %d, Week %d and day %d is not a valid date.';
  SErrInvalidDayOfWeekInMonth   = 'Year %d Month %d NDow %d DOW %d is not a valid date';
  SInvalidJulianDate            = '%f Julian cannot be represented as a DateTime';
  SErrInvalidHourMinuteSecMsec  = '%d:%d:%d.%d is not a valid time specification';

  SInvalidGUID                  = '"%s" is not a valid GUID value';

implementation

end.

