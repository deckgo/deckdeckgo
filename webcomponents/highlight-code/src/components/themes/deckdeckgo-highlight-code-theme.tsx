import {FunctionalComponent, h} from '@stencil/core';

export const Theme: FunctionalComponent<{style: string}> = ({style}) => {
  return (
    <style>{`
      :host ${style};
    `}</style>
  );
};

export function loadTheme(themeName: string | undefined): Promise<{theme} | undefined> {
  if (!themeName || themeName === undefined) {
    return undefined;
  }

  switch (themeName) {
    case '3024-night':
      return import(`../themes/3024-night`);
    case 'a11y-dark':
      return import(`../themes/a11y-dark`);
    case 'base16-dark':
      return import(`../themes/base16-dark`);
    case 'base16-light':
      return import(`../themes/base16-light`);
    case 'blackboard':
      return import(`../themes/blackboard`);
    case 'cobalt':
      return import(`../themes/cobalt`);
    case 'duotone':
      return import(`../themes/duotone`);
    case 'hopscotch':
      return import(`../themes/hopscotch`);
    case 'lucario':
      return import(`../themes/lucario`);
    case 'material':
      return import(`../themes/material`);
    case 'monokai':
      return import(`../themes/monokai`);
    case 'night-owl':
      return import(`../themes/night-owl`);
    case 'nord':
      return import(`../themes/nord`);
    case 'oceanic-next':
      return import(`../themes/oceanic-next`);
    case 'one-dark':
      return import(`../themes/one-dark`);
    case 'one-light':
      return import(`../themes/one-light`);
    case 'panda':
      return import(`../themes/panda`);
    case 'paraiso':
      return import(`../themes/paraiso`);
    case 'seti':
      return import(`../themes/seti`);
    case 'shades-of-purple':
      return import(`../themes/shades-of-purple`);
    case 'solarized-dark':
      return import(`../themes/solarized-dark`);
    case 'solarized-light':
      return import(`../themes/solarized-light`);
    case 'synthwave':
      return import(`../themes/synthwave`);
    case 'twilight':
      return import(`../themes/twilight`);
    case 'verminal':
      return import(`../themes/verminal`);
    case 'vscode':
      return import(`../themes/vscode`);
    case 'yeti':
      return import(`../themes/yeti`);
    case 'zenburn':
      return import(`../themes/zenburn`);
    default:
      return import(`../themes/dracula`);
  }
}
