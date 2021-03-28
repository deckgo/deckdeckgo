import React, {useEffect, useRef} from 'react';

import {FormattedMessage} from 'react-intl';

import {debounce} from '@deckdeckgo/utils';

import {section, main, deck} from './landing-poll.module.scss';

export const LandingPoll = () => {
  let pollObserver = undefined;

  const pollRef = useRef();

  useEffect(() => {
    if (!pollRef || !pollRef.current) {
      return;
    }

    if (window && 'IntersectionObserver' in window) {
      deferPollIntersectionObserverLoad();
    }

    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [pollRef]);

  const deferPollIntersectionObserverLoad = () => {
    pollObserver = new IntersectionObserver(onPollIntersection, {
      rootMargin: '100px 0px',
      threshold: 0.25,
    });

    pollObserver.observe(pollRef.current);
  };

  const onPollIntersection = async (entries) => {
    if (!entries || entries.length <= 0) {
      return;
    }

    await handlePollIntersection(entries[0]);
  };

  const handlePollIntersection = async (entry) => {
    if (entry.isIntersecting) {
      await loadPoll();
      pollObserver.unobserve(entry.target);
    }
  };

  const loadPoll = async () => {
    if (!pollRef || !pollRef.current) {
      return;
    }

    await pollRef.current.lazyLoadContent();

    pollRef.current.addEventListener(
      'pollUpdated',
      debounce(async () => {
        await pollRef.current.resizeContent();
      }),
      {once: true}
    );
  };

  const loadPollWithoutIntersectionObserver = async () => {
    if (window && 'IntersectionObserver' in window) {
      return;
    }

    await loadPoll();
  };

  return (
    <section className={section}>
      <main className={main}>
        <h2>
          <FormattedMessage id="hero.poll.description.title" />
        </h2>

        <p>
          <FormattedMessage id="hero.poll.description.content" />
        </p>

        {renderPollDemo()}
      </main>
    </section>
  );

  function renderPollDemo() {
    return (
      <div className={deck}>
        <deckgo-slide-poll
          ref={pollRef}
          onSlideDidLoad={async () => await loadPollWithoutIntersectionObserver()}
          class="showcase"
          style={{
            '--deckgo-qrcode-color-fill': '#222428',
            '--deckgo-chart-fill-color-1': 'var(--color-primary)',
            '--deckgo-chart-fill-color-2': 'var(--color-secondary)',
          }}>
          <h3 slot="question">
            <FormattedMessage id="hero.poll.description.slide.title" />
          </h3>
          <p slot="answer-1">
            <FormattedMessage id="hero.poll.description.slide.answer1" />
          </p>
          <p slot="answer-2">
            <FormattedMessage id="hero.poll.description.slide.answer2" />
          </p>
          <p slot="how-to">
            <FormattedMessage
              id="hero.poll.description.slide.howto"
              values={{
                pollLink: <a href="https://app.deckdeckgo.com/poll">app.deckdeckgo.com/poll</a>,
              }}
            />{' '}
            {'{0}'}
          </p>
          <p slot="awaiting-votes" style={{padding: '0.45rem'}}>
            <FormattedMessage id="hero.poll.description.slide.awaiting" />
          </p>
        </deckgo-slide-poll>
      </div>
    );
  }
};
