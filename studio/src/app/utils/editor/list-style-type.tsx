export enum ListStyle {
  DECIMAL = 'decimal',
  DECIMAL_LEADING = 'decimal-leading-zero',
  ROMAN_UPPER = 'upper-roman',
  ROMAN_LOWER = 'lower-roman',
  LATIN_UPPER = 'upper-latin',
  LATIN_LOWER = 'lower-latin',
  BULLET = 'disc',
  CIRCLE = 'circle',
  SQUARE = 'square',
}

export function mapHtmlListStyleTypeToListStyle(listType: string): ListStyle | undefined {
  switch (listType) {
    case 'decimal':
      return ListStyle.DECIMAL;
    case 'decimal-leading-zero':
      return ListStyle.DECIMAL_LEADING;
    case 'upper-roman':
      return ListStyle.ROMAN_UPPER;
    case 'lower-roman':
      return ListStyle.ROMAN_LOWER;
    case 'lower-latin':
      return ListStyle.LATIN_LOWER;
    case 'upper-latin':
      return ListStyle.LATIN_UPPER;
    case 'disc':
      return ListStyle.BULLET;
    case 'circle':
      return ListStyle.CIRCLE;
    case 'square':
      return ListStyle.SQUARE;
    default:
      return undefined;
  }
}
