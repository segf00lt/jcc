#ifndef CORE_C
#define CORE_C

#if 0
Dense_time dense_time_from_date_time(Date_time date_time) {
  Dense_time result = 0;
  result += date_time.year;
  result *= 12;
  result += date_time.month;
  result *= 31;
  result += date_time.day;
  result *= 24;
  result += date_time.hour;
  result *= 60;
  result += date_time.min;
  result *= 61;
  result += date_time.sec;
  result *= 1000;
  result += date_time.msec;
  return result;
}

Date_time date_time_from_dense_time(Dense_time time) {
  Date_time result = {0};
  result.msec = time%1000;
  time /= 1000;
  result.sec  = time%61;
  time /= 61;
  result.min  = time%60;
  time /= 60;
  result.hour = time%24;
  time /= 24;
  result.day  = time%31;
  time /= 31;
  result.month  = time%12;
  time /= 12;
  ASSERT(time <= MAX_U32);
  result.year = (u32)time;

  return result;
}

Date_time date_time_from_micro_seconds(u64 time) {
  Date_time result = {0};
  result.micro_sec = time%1000;
  time /= 1000;
  result.msec = time%1000;
  time /= 1000;
  result.sec = time%60;
  time /= 60;
  result.min = time%60;
  time /= 60;
  result.hour = time%24;
  time /= 24;
  result.day = time%31;
  time /= 31;
  result.month = time%12;
  time /= 12;
  ASSERT(time <= MAX_U32);
  result.year = (u32)time;
  return result;
}

Dense_time dense_time_from_unix_time(u64 unix_time) {
  // TODO make this more direct if possible
  Dense_time result = dense_time_from_date_time(date_time_from_unix_time(unix_time));
  return result;
}

Date_time date_time_from_unix_time(u64 unix_time) {
  Date_time date = {0};
  date.year     = 1970;
  date.day      = 1 + (unix_time / 86400);
  date.sec      = (u32)unix_time % 60;
  date.min      = (u32)(unix_time / 60) % 60;
  date.hour     = (u32)(unix_time / 3600) % 24;

  for(;;) {
    for(date.month = 0; date.month < 12; ++date.month) {
      u64 c = 0;
      switch(date.month) {
        case MONTH_JAN: c = 31; break;
        case MONTH_FEB:
        {
          if((date.year % 4 == 0) && ((date.year % 100) != 0 || (date.year % 400) == 0))
          {
            c = 29;
          }
          else
          {
            c = 28;
          }
        } break;
        case MONTH_MAR: c = 31; break;
        case MONTH_APR: c = 30; break;
        case MONTH_MAY: c = 31; break;
        case MONTH_JUN: c = 30; break;
        case MONTH_JUL: c = 31; break;
        case MONTH_AUG: c = 31; break;
        case MONTH_SEP: c = 30; break;
        case MONTH_OCT: c = 31; break;
        case MONTH_NOV: c = 30; break;
        case MONTH_DEC: c = 31; break;
        default: UNREACHABLE;
      }
      if(date.day <= c) {
        goto exit;
      }
      date.day -= c;
    }
    ++date.year;
  }
  exit:;

  return date;
}

#endif

#endif
