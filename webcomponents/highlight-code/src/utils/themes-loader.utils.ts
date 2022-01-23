import {DeckdeckgoHighlightCodeCarbonTheme} from '../declarations/carbon-theme';

export function loadTheme(theme: DeckdeckgoHighlightCodeCarbonTheme | undefined): Promise<{theme} | undefined> {
  if (!theme || theme === undefined) {
    return undefined;
  }

  switch (theme) {
    case DeckdeckgoHighlightCodeCarbonTheme['3024-NIGHT']:
      return import(`../components/themes/3024-night`);
    case DeckdeckgoHighlightCodeCarbonTheme['A11Y-DARK']:
      return import(`../components/themes/a11y-dark`);
    case DeckdeckgoHighlightCodeCarbonTheme['BASE16-DARK']:
      return import(`../components/themes/base16-dark`);
    case DeckdeckgoHighlightCodeCarbonTheme['BASE16-LIGHT']:
      return import(`../components/themes/base16-light`);
    case DeckdeckgoHighlightCodeCarbonTheme.BLACKBOARD:
      return import(`../components/themes/blackboard`);
    case DeckdeckgoHighlightCodeCarbonTheme.COBALT:
      return import(`../components/themes/cobalt`);
    case DeckdeckgoHighlightCodeCarbonTheme.DUOTONE:
      return import(`../components/themes/duotone`);
    case DeckdeckgoHighlightCodeCarbonTheme.HOPSCOTCH:
      return import(`../components/themes/hopscotch`);
    case DeckdeckgoHighlightCodeCarbonTheme.LUCARIO:
      return import(`../components/themes/lucario`);
    case DeckdeckgoHighlightCodeCarbonTheme.MATERIAL:
      return import(`../components/themes/material`);
    case DeckdeckgoHighlightCodeCarbonTheme.MONOKAI:
      return import(`../components/themes/monokai`);
    case DeckdeckgoHighlightCodeCarbonTheme['NIGHT-OWL']:
      return import(`../components/themes/night-owl`);
    case DeckdeckgoHighlightCodeCarbonTheme.NORD:
      return import(`../components/themes/nord`);
    case DeckdeckgoHighlightCodeCarbonTheme['OCEANIC-NEXT']:
      return import(`../components/themes/oceanic-next`);
    case DeckdeckgoHighlightCodeCarbonTheme['ONE-DARK']:
      return import(`../components/themes/one-dark`);
    case DeckdeckgoHighlightCodeCarbonTheme['ONE-LIGHT']:
      return import(`../components/themes/one-light`);
    case DeckdeckgoHighlightCodeCarbonTheme.PANDA:
      return import(`../components/themes/panda`);
    case DeckdeckgoHighlightCodeCarbonTheme.PARAISO:
      return import(`../components/themes/paraiso`);
    case DeckdeckgoHighlightCodeCarbonTheme.SETI:
      return import(`../components/themes/seti`);
    case DeckdeckgoHighlightCodeCarbonTheme['SHADES-OF-PURPLE']:
      return import(`../components/themes/shades-of-purple`);
    case DeckdeckgoHighlightCodeCarbonTheme['SOLARIZED-DARK']:
      return import(`../components/themes/solarized-dark`);
    case DeckdeckgoHighlightCodeCarbonTheme['SOLARIZED-LIGHT']:
      return import(`../components/themes/solarized-light`);
    case DeckdeckgoHighlightCodeCarbonTheme.SYNTHWAVE:
      return import(`../components/themes/synthwave`);
    case DeckdeckgoHighlightCodeCarbonTheme.TWILIGHT:
      return import(`../components/themes/twilight`);
    case DeckdeckgoHighlightCodeCarbonTheme.VERMINAL:
      return import(`../components/themes/verminal`);
    case DeckdeckgoHighlightCodeCarbonTheme.VSCODE:
      return import(`../components/themes/vscode`);
    case DeckdeckgoHighlightCodeCarbonTheme.YETI:
      return import(`../components/themes/yeti`);
    case DeckdeckgoHighlightCodeCarbonTheme.ZENBURN:
      return import(`../components/themes/zenburn`);
    default:
      return import(`../components/themes/dracula`);
  }
}
