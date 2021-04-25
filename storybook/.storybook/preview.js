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
        'Components',
        ['Demo', 'Drag Resize Rotate', 'Highlight Code', 'Inline Editor', 'Laser Pointer', 'Lazy Image', 'Math', 'QR Code', 'Pie Chart', 'Bar Chart', 'Line Chart', 'Social', 'Word Cloud'],
        'Miscellaneous',
        ['Contact', 'Sponsor', 'Logo'],
      ]
    }
  },
  docs: {
    theme
  }
};
