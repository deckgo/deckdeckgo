import {DeckdeckgoCarbonTheme} from '../declarations/deckdeckgo-highlight-code-carbon-theme';

export function loadTheme(theme: DeckdeckgoCarbonTheme | undefined): Promise<{theme} | undefined> {
  if (!theme || theme === undefined) {
    return undefined;
  }

  switch (theme) {
    case DeckdeckgoCarbonTheme['3024-NIGHT']:
      return import(`../components/themes/3024-night`);
    case DeckdeckgoCarbonTheme['A11Y-DARK']:
      return import(`../components/themes/a11y-dark`);
    case DeckdeckgoCarbonTheme['BASE16-DARK']:
      return import(`../components/themes/base16-dark`);
    case DeckdeckgoCarbonTheme['BASE16-LIGHT']:
      return import(`../components/themes/base16-light`);
    case DeckdeckgoCarbonTheme.BLACKBOARD:
      return import(`../components/themes/blackboard`);
    case DeckdeckgoCarbonTheme.COBALT:
      return import(`../components/themes/cobalt`);
    case DeckdeckgoCarbonTheme.DUOTONE:
      return import(`../components/themes/duotone`);
    case DeckdeckgoCarbonTheme.HOPSCOTCH:
      return import(`../components/themes/hopscotch`);
    case DeckdeckgoCarbonTheme.LUCARIO:
      return import(`../components/themes/lucario`);
    case DeckdeckgoCarbonTheme.MATERIAL:
      return import(`../components/themes/material`);
    case DeckdeckgoCarbonTheme.MONOKAI:
      return import(`../components/themes/monokai`);
    case DeckdeckgoCarbonTheme['NIGHT-OWL']:
      return import(`../components/themes/night-owl`);
    case DeckdeckgoCarbonTheme.NORD:
      return import(`../components/themes/nord`);
    case DeckdeckgoCarbonTheme['OCEANIC-NEXT']:
      return import(`../components/themes/oceanic-next`);
    case DeckdeckgoCarbonTheme['ONE-DARK']:
      return import(`../components/themes/one-dark`);
    case DeckdeckgoCarbonTheme['ONE-LIGHT']:
      return import(`../components/themes/one-light`);
    case DeckdeckgoCarbonTheme.PANDA:
      return import(`../components/themes/panda`);
    case DeckdeckgoCarbonTheme.PARAISO:
      return import(`../components/themes/paraiso`);
    case DeckdeckgoCarbonTheme.SETI:
      return import(`../components/themes/seti`);
    case DeckdeckgoCarbonTheme['SHADES-OF-PURPLE']:
      return import(`../components/themes/shades-of-purple`);
    case DeckdeckgoCarbonTheme['SOLARIZED-DARK']:
      return import(`../components/themes/solarized-dark`);
    case DeckdeckgoCarbonTheme['SOLARIZED-LIGHT']:
      return import(`../components/themes/solarized-light`);
    case DeckdeckgoCarbonTheme.SYNTHWAVE:
      return import(`../components/themes/synthwave`);
    case DeckdeckgoCarbonTheme.TWILIGHT:
      return import(`../components/themes/twilight`);
    case DeckdeckgoCarbonTheme.VERMINAL:
      return import(`../components/themes/verminal`);
    case DeckdeckgoCarbonTheme.VSCODE:
      return import(`../components/themes/vscode`);
    case DeckdeckgoCarbonTheme.YETI:
      return import(`../components/themes/yeti`);
    case DeckdeckgoCarbonTheme.ZENBURN:
      return import(`../components/themes/zenburn`);
    default:
      return import(`../components/themes/dracula`);
  }
}
