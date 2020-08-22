export interface GitHubOwner {
  login: string; // 'fluster' or 'peterpeterparker'
  id: number; // 40920969 or 16886711
  node_id: string; // 'MDEyOk9yZ2FuaXphdGlvbjQwOTIwOTY5' or 'MDQ6VXNlcjE2ODg2NzEx'
}

export interface GitHubForkParent {
  id: number; // 129246547
  node_id: string; // 'MDEwOlJlcG9zaXRvcnkxMjkyNDY1NDc'
  name: string; // 'web-social-share'
  private: boolean; // false
  owner: GitHubOwner;
}

export interface GitHubForkResponse {
  id: number;
  node_id: string; // own repo node id
  name: string; // 'web-social-share'
  full_name: string; // 'peterpeterparker/web-social-share'
  html_url: string; // 'https://github.com/peterpeterparker/web-social-share'
  url: string; // 'https://api.github.com/repos/peterpeterparker/web-social-share'
  git_url: string; // 'git://github.com/fluster/web-social-share.git'
  ssh_url: string; // 'git@github.com:fluster/web-social-share.git'
  clone_url: string; // 'https://github.com/fluster/web-social-share.git'
  owner: GitHubOwner;
  parent: GitHubForkParent;
}
