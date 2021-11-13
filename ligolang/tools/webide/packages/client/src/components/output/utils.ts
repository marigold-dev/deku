export function copyOutput(el: HTMLElement | null) {
  if (el) {
    const range = document.createRange();
    range.selectNodeContents(el);

    const selection = window.getSelection();

    if (selection) {
      selection.removeAllRanges();
      selection.addRange(range);
      document.execCommand('copy');
    }
  }
}

export function downloadOutput(output: string) {
  const anchor = document.createElement('a');
  anchor.setAttribute(
    'href',
    `data:text/plain;charset=utf-8,${encodeURIComponent(output)}`
  );
  anchor.setAttribute('download', 'output.txt');

  anchor.style.display = 'none';
  document.body.appendChild(anchor);
  anchor.click();
  document.body.removeChild(anchor);
}
