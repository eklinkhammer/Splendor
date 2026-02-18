Deploy the Splendor application to the production server.

## Instructions

Run the production deploy workflow by SSHing into the server and executing the deploy steps. This mirrors the GitHub Actions deploy job.

### Deploy steps:
1. SSH into the server and pull latest code:
   ```
   ssh -i ~/.ssh/EricMacRumble.pem ubuntu@207.5.207.232 'cd ~/Splendor && git pull origin master'
   ```

2. Rebuild and restart containers:
   ```
   ssh -i ~/.ssh/EricMacRumble.pem ubuntu@207.5.207.232 'cd ~/Splendor && docker compose up --build -d'
   ```

3. Reload nginx:
   ```
   ssh -i ~/.ssh/EricMacRumble.pem ubuntu@207.5.207.232 'sudo systemctl reload nginx'
   ```

4. Clean up old images:
   ```
   ssh -i ~/.ssh/EricMacRumble.pem ubuntu@207.5.207.232 'docker image prune -f'
   ```

### Important:
- Run each step sequentially and check for errors before proceeding to the next
- If any step fails, stop and report the error to the user
- Do NOT run any other commands beyond the deploy workflow
- This should only be run when the user explicitly wants to deploy

$ARGUMENTS
