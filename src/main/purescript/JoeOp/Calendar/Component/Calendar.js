export const focusId = id => {
  return () => {
    const el = document.getElementById(id);
    if (el) {
      el.focus();
    }
  }
}
