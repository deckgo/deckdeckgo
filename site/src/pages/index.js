/**
 * Do not modify! This file is overwritten by index.en.js at build time.
 */
import * as React from 'react';

import Layout from '../components/core/layout/en';

import {Hero} from '../components/landing/hero/hero';
import {LandingDeck} from '../components/landing/deck/landing-deck';
import {LandingContent} from '../components/landing/content/landing-content';
import {Footer} from '../components/core/footer/footer';

const IndexPage = (props) => {
  return (
    <Layout location={props.location} sticky={false}>
      <Hero />

      <LandingDeck />

      <LandingContent />

      <Footer lang="en" />
    </Layout>
  );
};

export default IndexPage;
