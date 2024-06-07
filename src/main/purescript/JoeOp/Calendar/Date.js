const luxon = require('luxon');

const padNumber = (num,len) => {
  let numStr = num.toString();
  while (numStr.length < len) {
    numStr = "0" + numStr;
  }
  return numStr;
};

export const _monthStartsOnWeekDay = year => month => {
  let dt = luxon.DateTime.fromISO(`${year.toString()}-${padNumber(month, 2)}-01`);
  let luxonWeekday = dt.weekday;
  // -1 to convert 1-indexed to 0-indexed
  // +1 to make Sunday the first day of the week
  // %7
  return luxonWeekday % 7;
}

export const _today = toUnwrappedDate => {
  return () => {
    const today = luxon.DateTime.local();
    return toUnwrappedDate(today.year)(today.month)(today.day);
  };
};
