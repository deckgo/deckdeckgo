import {DeckdeckgoCarbonTheme} from '../declarations/deckdeckgo-highlight-code-carbon-theme';

export function loadTheme(theme: DeckdeckgoCarbonTheme | undefined): Promise<{theme} | undefined> {
  if (!theme || theme === undefined) {
    return undefined;
  }

  switch (theme) {
    case '3024-night':
      return import(`../components/themes/3024-night`);
    case 'a11y-dark':
      return import(`../components/themes/a11y-dark`);
    case 'base16-dark':
      return import(`../components/themes/base16-dark`);
    case 'base16-light':
      return import(`../components/themes/base16-light`);
    case 'blackboard':
      return import(`../components/themes/blackboard`);
    case 'cobalt':
      return import(`../components/themes/cobalt`);
    case 'duotone':
      return import(`../components/themes/duotone`);
    case 'hopscotch':
      return import(`../components/themes/hopscotch`);
    case 'lucario':
      return import(`../components/themes/lucario`);
    case 'material':
      return import(`../components/themes/material`);
    case 'monokai':
      return import(`../components/themes/monokai`);
    case 'night-owl':
      return import(`../components/themes/night-owl`);
    case 'nord':
      return import(`../components/themes/nord`);
    case 'oceanic-next':
      return import(`../components/themes/oceanic-next`);
    case 'one-dark':
      return import(`../components/themes/one-dark`);
    case 'one-light':
      return import(`../components/themes/one-light`);
    case 'panda':
      return import(`../components/themes/panda`);
    case 'paraiso':
      return import(`../components/themes/paraiso`);
    case 'seti':
      return import(`../components/themes/seti`);
    case 'shades-of-purple':
      return import(`../components/themes/shades-of-purple`);
    case 'solarized-dark':
      return import(`../components/themes/solarized-dark`);
    case 'solarized-light':
      return import(`../components/themes/solarized-light`);
    case 'synthwave':
      return import(`../components/themes/synthwave`);
    case 'twilight':
      return import(`../components/themes/twilight`);
    case 'verminal':
      return import(`../components/themes/verminal`);
    case 'vscode':
      return import(`../components/themes/vscode`);
    case 'yeti':
      return import(`../components/themes/yeti`);
    case 'zenburn':
      return import(`../components/themes/zenburn`);
    default:
      return import(`../components/themes/dracula`);
  }
}
