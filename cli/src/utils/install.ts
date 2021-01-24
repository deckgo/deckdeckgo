import { Spinner } from "cli-spinner";
import { bold } from "colorette";

import { npm } from "./utils";

export const installDependencies = async (folder: string, step: string) => {
  const loading = new Spinner(bold(`[${step}] Installing dependencies...`));
  loading.setSpinnerString(18);
  loading.start();

  await npm('ci', folder);

  loading.stop(true);
}
