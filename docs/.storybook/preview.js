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
        ['Templates', 'Title', 'Content', 'Split', 'Poll', 'Aspect Ratio', 'GIF', 'Chart', 'Youtube', 'Author', 'QR Code', 'Playground'],
        'Components',
        [
          'Color',
          'Demo',
          'Drag Resize Rotate',
          'Highlight Code',
          'Laser Pointer',
          'Lazy Image',
          'Math',
          'Pie Chart',
          'Bar Chart',
          'Line Chart',
          'QR Code',
          'Social',
          'Social Image',
          'Word Cloud',
          'Youtube'
        ],
        'Deck',
        ['Animation', 'Auto Slide', 'Background', 'Header Footer', 'Navigation', 'Pager', 'Size', 'More Features', 'Events', 'Actions'],
        'Miscellaneous',
        ['Contact', 'Sponsor', 'Logo']
      ]
    }
  },
  docs: {
    theme
  }
};
