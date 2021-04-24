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
        ['BarChart'],
        'Miscellaneous',
        ['Contact', 'Sponsor', 'Logo'],
      ]
    }
  },
  docs: {
    theme
  }
};
