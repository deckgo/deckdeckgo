// TODO: Duplicate related WebComponents to reduce JS chunks. Ultimately move this to a utilities

export interface DeckdeckgoPaletteColor {
  hex: string;
  rgb?: string;
}

export interface DeckdeckgoPaletteDisplayColor {
  borderColor: string;
  boxShadowColor: string;
}

export interface DeckdeckgoPalette {
  color: DeckdeckgoPaletteColor;
  alt?: string;
  display?: DeckdeckgoPaletteDisplayColor;
}

export const DEFAULT_PALETTE: DeckdeckgoPalette[] = [
  {
    color: {
      hex: '#8ED1FC',
      rgb: '142,209,252'
    },
    alt: 'Light blue'
  },
  {
    color: {
      hex: '#0693E3',
      rgb: '6,147,227'
    },
    alt: 'Blue'
  },
  {
    color: {
      hex: '#7BDCB5',
      rgb: '123,220,181'
    },
    alt: 'Light green'
  },
  {
    color: {
      hex: '#00D084',
      rgb: '0,208,132'
    },
    alt: 'Green'
  },
  {
    color: {
      hex: '#FCB900',
      rgb: '252,185,0'
    },
    alt: 'Yellow'
  },
  {
    color: {
      hex: '#FF6900',
      rgb: '255,105,0'
    },
    alt: 'Orange'
  },
  {
    color: {
      hex: '#F78DA7',
      rgb: '247,141,167'
    },
    alt: 'Pink'
  },
  {
    color: {
      hex: '#EB144C',
      rgb: '235,20,76'
    },
    alt: 'Red'
  },
  {
    color: {
      hex: '#ffffff',
      rgb: '255,255,255'
    },
    alt: 'White',
    display: {
      borderColor: '#ddd',
      boxShadowColor: '221,221,221'
    }
  },
  {
    color: {
      hex: '#ABB8C3',
      rgb: '171,184,195'
    },
    alt: 'Grey'
  },
  {
    color: {
      hex: '#000000',
      rgb: '0,0,0'
    },
    alt: 'Black'
  }
];
