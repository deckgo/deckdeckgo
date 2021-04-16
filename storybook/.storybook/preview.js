export const parameters = {
  actions: {argTypesRegex: '^on[A-Z].*'},
  controls: {
    matchers: {
      color: /(background|color)$/i,
      date: /Date$/
    }
  },
  options: {
    storySort: {
      order: ['Introduction', ['Introduction', 'Getting Started', 'Installation', 'Showcase', 'Publish', 'Remote Control'], 'Components']
    }
  }
};
