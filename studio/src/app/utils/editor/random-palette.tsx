export interface PaletteColor {
  hex: string;
  rgba: string;
}

export interface Palette {
  color: PaletteColor;
  contrast: PaletteColor;
}

export const DEFAULT_DECK_PALETTE: Palette[] = [
  {
    color: {
      hex: '#ffffff',
      rgba: 'rgba(255,255,255,1)'
    },
    contrast: {
      hex: '#000000',
      rgba: 'rgba(0,0,0,1)'
    }
  },
  {
    color: {
      hex: '#2a324b',
      rgba: 'rgba(42,50,75,1)'
    },
    contrast: {
      hex: '#ffffff',
      rgba: 'rgba(255,255,255,1)'
    }
  },
  {
    color: {
      hex: '#2a324b',
      rgba: 'rgba(42,50,75,1)'
    },
    contrast: {
      hex: '#ffffff',
      rgba: 'rgba(255,255,255,1)'
    }
  },
  {
    color: {
      hex: '#0694a8',
      rgba: 'rgba(6,148,168,1)'
    },
    contrast: {
      hex: '#ffffff',
      rgba: 'rgba(255,255,255,1)'
    }
  },
  {
    color: {
      hex: '#3b3637',
      rgba: 'rgba(59,54,55,1)'
    },
    contrast: {
      hex: '#e0ffff',
      rgba: 'rgba(224,255,255,1)'
    }
  },
  {
    color: {
      hex: '#f78da7',
      rgba: 'rgba(247,141,167,1)'
    },
    contrast: {
      hex: '#000000',
      rgba: 'rgba(0,0,0,1)'
    }
  },
  {
    color: {
      hex: '#0693e3',
      rgba: 'rgba(6,147,227,1)'
    },
    contrast: {
      hex: '#ffffff',
      rgba: 'rgba(255,255,255,1)'
    }
  },
  {
    color: {
      hex: '#9900ef',
      rgba: 'rgba(153,0,239,1)'
    },
    contrast: {
      hex: '#ffffff',
      rgba: 'rgba(255,255,255,1)'
    }
  },
  {
    color: {
      hex: '#7bdcb5',
      rgba: 'rgba(123,220,181,1)'
    },
    contrast: {
      hex: '#000000',
      rgba: 'rgba(0,0,0,1)'
    }
  }
];

export function generateRandomStyleColors(): Promise<any | undefined> {
  return new Promise<any | undefined>((resolve) => {
    if (!DEFAULT_DECK_PALETTE || DEFAULT_DECK_PALETTE.length <= 0) {
      resolve(undefined);
      return;
    }

    const index: number = Math.floor(Math.random() * DEFAULT_DECK_PALETTE.length);

    resolve({
      '--color': DEFAULT_DECK_PALETTE[index].contrast.rgba,
      '--background': DEFAULT_DECK_PALETTE[index].color.rgba
    });
  });
}
