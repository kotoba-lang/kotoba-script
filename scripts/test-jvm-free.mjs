import { readFileSync, mkdtempSync, writeFileSync, existsSync, rmSync } from 'node:fs';
import { dirname, resolve, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { tmpdir } from 'node:os';
import { spawnSync } from 'node:child_process';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const peers = JSON.parse(readFileSync(join(root, 'scripts/jvm-free-peers.json'), 'utf8'));
const peersRoot = process.env.KOTOBA_TEST_PEERS_ROOT || resolve(root, '..');
const paths = [join(root, 'src')];
for (const [name, expected] of Object.entries(peers)) {
  const path = join(peersRoot, name);
  const result = spawnSync('git', ['-C', path, 'rev-parse', 'HEAD'], { encoding: 'utf8' });
  if (result.status !== 0 || result.stdout.trim() !== expected) {
    throw new Error(`Missing/stale test peer ${name}; expected ${expected} at ${path}`);
  }
  paths.push(join(path, 'src'));
}

const temp = mkdtempSync(join(tmpdir(), 'kotoba-jvm-free-'));
const marker = join(temp, 'jvm-invoked');
try {
  // Detect attempted JVM fallback even if a caller catches its failure.
  for (const name of ['java', 'javac', 'clojure', 'clj']) {
    writeFileSync(join(temp, name), '#!/bin/sh\n: > "$KOTOBA_JVM_MARKER"\nexit 97\n', { mode: 0o755 });
  }
  for (const test of ['test/nbb/fuel.cljk', 'test/nbb/differential.cljk']) {
    const result = spawnSync(process.execPath,
      [join(root, 'node_modules/nbb/cli.js'), '--classpath', paths.join(':'), test],
      { cwd: root, stdio: 'inherit', timeout: 120000,
        env: { ...process.env, TMPDIR: temp, KOTOBA_JVM_MARKER: marker,
          PATH: `${temp}:${process.env.PATH || ''}` } });
    if (existsSync(marker)) throw new Error(`JVM executable invoked by ${test}`);
    if (result.error || result.status !== 0) throw new Error(`${test} failed: ${result.error || result.status}`);
  }
  console.log('JVM-free acceptance: 44 assertions; no JVM fallback observed.');
} finally {
  rmSync(temp, { recursive: true, force: true });
}
