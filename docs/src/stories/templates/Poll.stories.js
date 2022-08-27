import {DocPoll} from './Readme.mdx';

export default {
  title: 'Templates/Poll',
  parameters: {
    docs: {
      page: DocPoll
    }
  },
  argTypes: {
    pollLink: {control: 'text'},
    socketUrl: {control: 'text'}
  }
};

export const Poll = ({pollLink, socketUrl}) => {
  const simpleLink = pollLink.replace('https://', '');
  return `<div class="container">
  <deckgo-deck embedded="true">
    <deckgo-slide-poll poll-link="${pollLink}" socket-url="${socketUrl}">
        <h1 slot="question">Do you like my presentation so far?</h1>
        <p slot="answer-1">It is super</p>
        <p slot="answer-2">Meh</p>
        <p slot="answer-3">I could'nt care less</p>
        <p slot="answer-4">Tell me why</p>
        <p slot="answer-5">Ain't nothin' but a heartache</p>
        <p slot="how-to">Go to <a href="${pollLink}">${simpleLink}</a> and use the code {0}</p>
        <p slot="awaiting-votes">Awaiting first votes</p>
      </deckgo-slide-poll>
  </deckgo-deck>
</div>`;
};

Poll.args = {
  pollLink: 'https://app.deckdeckgo.com/poll',
  socketUrl: 'https://api.deckdeckgo.com'
};
