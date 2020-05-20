import {FunctionalComponent, h} from '@stencil/core';

export const Theme: FunctionalComponent<{style: string}> = ({style}) => {
  return (
    <style>{`
      :host ${style};
    `}</style>
  );
};

export async function loadTheme(themeName: string | undefined): Promise<{theme} | undefined> {
  if (!themeName || themeName === undefined) {
    return undefined;
  }

  switch (themeName) {
    case '3024-night':
      return await import(`../themes/3024-night`);
    case 'a11y-dark':
      return await import(`../themes/a11y-dark`);
    case 'base16-dark':
      return await import(`../themes/base16-dark`);
    case 'base16-light':
      return await import(`../themes/base16-light`);
    case 'blackboard':
      return await import(`../themes/blackboard`);
    case 'cobalt':
      return await import(`../themes/cobalt`);
    case 'duotone':
      return await import(`../themes/duotone`);
    case 'hopscotch':
      return await import(`../themes/hopscotch`);
    case 'lucario':
      return await import(`../themes/lucario`);
    case 'material':
      return await import(`../themes/material`);
    case 'monokai':
      return await import(`../themes/monokai`);
    case 'night-owl':
      return await import(`../themes/night-owl`);
    case 'nord':
      return await import(`../themes/nord`);
    case 'oceanic-next':
      return await import(`../themes/oceanic-next`);
    case 'one-dark':
      return await import(`../themes/one-dark`);
    case 'one-light':
      return await import(`../themes/one-light`);
    case 'panda':
      return await import(`../themes/panda`);
    case 'paraiso':
      return await import(`../themes/paraiso`);
    case 'seti':
      return await import(`../themes/seti`);
    case 'shades-of-purple':
      return await import(`../themes/shades-of-purple`);
    case 'solarized-dark':
      return await import(`../themes/solarized-dark`);
    case 'solarized-light':
      return await import(`../themes/solarized-light`);
    case 'synthwave':
      return await import(`../themes/synthwave`);
    case 'twilight':
      return await import(`../themes/twilight`);
    case 'verminal':
      return await import(`../themes/verminal`);
    case 'vscode':
      return await import(`../themes/vscode`);
    case 'yeti':
      return await import(`../themes/yeti`);
    case 'zenburn':
      return await import(`../themes/zenburn`);
    default:
      return await import(`../themes/dracula`);
  }
}
