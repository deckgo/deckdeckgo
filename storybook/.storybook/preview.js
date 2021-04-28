import theme from './theme';

export const parameters = {
  actions: {argTypesRegex: '^on[A-Z].*'},
  options: {
    storySort: {
      order: [
        'Introduction',
        ['Introduction', 'Getting Started', 'Installation', 'Showcase', 'Publish', 'Remote Control'],
        'Edit',
        ['HTML', 'Lazy Loading', 'Theming', 'Fonts', 'Reveal', 'RTL', 'Notes'],
        'Templates',
        ['Templates', 'Title', 'Content', 'Split', 'Poll', 'Aspect Ratio', 'GIF', 'Chart', 'Youtube', 'Code', 'Author', 'QR Code', 'Playground'],
        'Components',
        ['Demo', 'Drag Resize Rotate', 'Highlight Code', 'Inline Editor', 'Laser Pointer', 'Lazy Image', 'Math', 'QR Code', 'Pie Chart', 'Bar Chart', 'Line Chart', 'Social', 'Word Cloud', 'Youtube'],
        'Deck',
        ['Animation', 'Auto Slide', 'Background', 'Header Footer', 'Navigation', 'Pager'],
        'Miscellaneous',
        ['Contact', 'Sponsor', 'Logo'],
      ]
    }
  },
  docs: {
    theme
  }
};
