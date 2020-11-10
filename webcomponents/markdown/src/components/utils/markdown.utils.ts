import marked from 'marked';

export function changeCodeCreation(renderer: marked.Renderer) {
  renderer.code = (code, lang, _escaped) => {
    const hcl = [];
    code = code
      .split('\n')
      .map((line: string, index: number) => {
        if (line.charAt(0) === '|') {
          hcl.push(index + 1);
          return line.substring(1);
        }
        return escapeUnsafe(line);
      })
      .join('\n');

    return `<deckgo-highlight-code language="${lang ? lang : 'javascript'}">
      <code slot="code">${code}</code>
    </deckgo-highlight-code>`;
  };
}

function escapeUnsafe(unsafe: string): string {
  return unsafe
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;')
    .replace(/\//g, '&#47;')
    .replace(/\\/g, '&#092;')
    .replace(/{/g, '&#123;')
    .replace(/}/g, '&#125;');
}
