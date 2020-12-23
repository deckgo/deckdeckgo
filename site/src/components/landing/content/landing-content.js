import React, {useEffect, useRef, useState} from 'react';

import {FormattedMessage, useIntl} from 'react-intl';

import {debounce} from '@deckdeckgo/utils';

import styles from './landing-content.module.scss';

import {LandingPoll} from '../poll/landing-poll';

export const LandingContent = () => {
  const intl = useIntl();

  const [videoWidth, setVideoWidth] = useState(undefined);
  const [videoHeight, setVideoHeight] = useState(undefined);

  const videoWidthRef = useRef(videoWidth);
  const videoHeightRef = useRef(videoHeight);

  const setSizeStates = (width, height) => {
    videoWidthRef.current = width;
    videoHeightRef.current = height;
    setVideoWidth(width);
    setVideoHeight(height);
  };

  const mainRef = useRef();

  const debounceOnWindowResize = debounce(async () => {
    await onWindowResize();
  });

  useEffect(() => {
    if (!mainRef || !mainRef.current) {
      return;
    }

    initVideoSize();

    window.addEventListener('resize', debounceOnWindowResize, {passive: true});

    return () => window.removeEventListener('resize', debounceOnWindowResize, false);

    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [mainRef]);

  const onWindowResize = async () => {
    await initVideoSize();

    const elements = document.querySelectorAll('deckgo-youtube');

    if (elements && elements.length > 0) {
      for (const element of Array.from(elements)) {
        await element.updateIFrame(videoWidthRef.current, videoHeightRef.current);
      }
    }
  };

  const initVideoSize = async () => {
    const mainWidth = mainRef.current.offsetWidth;

    const mediaQuery = window.matchMedia('(min-width: 992px)');

    const width = mediaQuery.matches ? mainWidth / 2 : mainWidth - 32;
    const height = (width * 9) / 16;

    setSizeStates(width, height);
  };

  let videoObserver = undefined;

  useEffect(() => {
    if (!videoHeight) {
      return;
    }

    if (window && 'IntersectionObserver' in window) {
      deferVideoIntersectionObserverLoad();
    } else {
      unfortunatelyLoadVideoNow();
    }

    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [videoHeight]);

  const deferVideoIntersectionObserverLoad = () => {
    videoObserver = new IntersectionObserver(onVideoIntersection, {
      rootMargin: '100px 0px',
      threshold: 0.25,
    });

    const elements = document.querySelectorAll('div.video');

    if (elements) {
      Array.from(elements).forEach((element) => {
        videoObserver.observe(element);
      });
    }
  };

  const unfortunatelyLoadVideoNow = async () => {
    const elements = document.querySelectorAll('deckgo-youtube');

    if (elements && elements.length > 0) {
      const promises = Array.from(elements).map((element) => element.lazyLoadContent());
      await Promise.all(promises);
    }
  };

  const onVideoIntersection = async (entries) => {
    if (!entries || entries.length <= 0) {
      return;
    }

    await handleVideoIntersection(entries);
  };

  const handleVideoIntersection = async (entries) => {
    for (const entry of entries) {
      if (entry.isIntersecting) {
        if (videoObserver && entry.target) {
          await entry.target.firstChild.lazyLoadContent();
          videoObserver.unobserve(entry.target);
        }
      }
    }
  };

  return (
    <>
      <section>
        <main className={styles.main} ref={mainRef}>
          {renderDescription()}
        </main>
      </section>

      <LandingPoll />

      <section>
        <main className={`${styles.main} ${styles.remote}`}>{renderRemote()}</main>
      </section>
    </>
  );

  function renderDescription() {
    return (
      <>
        <div>
          <h2>
            <FormattedMessage id="hero.content.description.title" />
          </h2>

          <p>
            <FormattedMessage id="hero.content.description.content" />
          </p>

          <p>
            <FormattedMessage id="hero.content.description.comparison" />
          </p>

          <p>
            <FormattedMessage
              id="hero.content.description.github"
              values={{
                githubLink: (
                  <a href="https://github.com/deckgo/deckdeckgo" rel="noopener noreferrer">
                    GitHub
                  </a>
                ),
              }}
            />
          </p>
        </div>

        {renderVideo('https://www.youtube.com/embed/Y97mEj9ZYmE', 'hero.content.description.alt')}
      </>
    );
  }

  function renderRemote() {
    return (
      <>
        {renderVideo('https://www.youtube.com/embed/PnSNT5WpauE', 'hero.content.remote.alt')}

        <div>
          <h2>
            <FormattedMessage id="hero.content.remote.title" />
          </h2>

          <p>
            <FormattedMessage id="hero.content.remote.content" />
          </p>
        </div>
      </>
    );
  }

  function renderVideo(url, alt) {
    return (
      <div className={`${styles.video} video`} style={{width: `${videoWidth}px`, height: `${videoHeight}px`}}>
        <deckgo-youtube width={videoWidth} height={videoHeight} src={url} frameTitle={intl.formatMessage({id: alt})}></deckgo-youtube>
      </div>
    );
  }
};
