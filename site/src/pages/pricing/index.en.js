import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';

const PricingPages = (props) => {
  return (
    <Layout location={props.location} sticky={false}>

      Free with Sponsorship

      Enterprise

      <Footer lang="en" action={false} />
    </Layout>
  );
};

export default PricingPages;
