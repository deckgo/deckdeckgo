export enum OrderedStyle {
  DECIMAL = 'decimal',
  DECIMAL_LEADING = 'decimal-leading-zero',
  ROMAN_UPPER = 'upper-roman',
  ROMAN_LOWER = 'lower-roman',
  LATIN_UPPER = 'upper-latin',
  LATIN_LOWER = 'lower-latin',
}

export enum UnorderedStyle {
  BULLET = 'disc',
  CIRCLE = 'circle',
  SQUARE = 'square',
}

export function mapHtmlListStyleTypeToListStyle(listType: string): OrderedStyle | UnorderedStyle | undefined {
  switch (listType) {
    case 'decimal':
      return OrderedStyle.DECIMAL;
    case 'decimal-leading-zero':
      return OrderedStyle.DECIMAL_LEADING;
    case 'upper-roman':
      return OrderedStyle.ROMAN_UPPER;
    case 'lower-roman':
      return OrderedStyle.ROMAN_LOWER;
    case 'lower-latin':
      return OrderedStyle.LATIN_LOWER;
    case 'upper-latin':
      return OrderedStyle.LATIN_UPPER;
    case 'disc':
      return UnorderedStyle.BULLET;
    case 'circle':
      return UnorderedStyle.CIRCLE;
    case 'square':
      return UnorderedStyle.SQUARE;
    default:
      return undefined;
  }
}
