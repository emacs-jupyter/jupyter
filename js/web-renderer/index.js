const puppeteer = require('puppeteer-core');
var sharp = require('sharp')

// TODO Add browser path arg
// TODO Support using headless firefox to take the screenshot
// TODO Check default locations for other operating systems
// TODO Gracefully handle case where Chrome is not found
const MACOS_CHROME_PATH =
  '/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome';
const REQUIRE_JS_URL = "https://requirejs.org/docs/release/2.3.6/minified/require.js";
const TIMEOUT = 800;

// Get js/html from emacs
var inputChunks = [];

process.stdin.on('readable', () => {
  let chunk;
  // Use a loop to make sure we read all available data.
  while ((chunk = process.stdin.read()) !== null) {
    inputChunks.push(chunk);
  }
});

process.stdin.on('end', () => {
  const body = Buffer.concat(inputChunks);

  (async () => {
    const browser = await puppeteer.launch({ executablePath: MACOS_CHROME_PATH });
    const page = await browser.newPage();

    const html =
     `<!DOCTYPE html>
      <html>
        <head>
          <script type="application/javascript"
                  src="${REQUIRE_JS_URL}">
          </script>
        </head>
        <body>
          ${body}
        </body>
      </html>`;

    await page.setContent(html);
    await page.waitFor(TIMEOUT);

    const rawImage = await page.screenshot();
    const croppedImage = await sharp(rawImage).trim().toBuffer()

    await process.stdout.write(croppedImage);
    await browser.close();
  })();
})
