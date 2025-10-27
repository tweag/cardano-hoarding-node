# Secrets Management with SOPS

This project uses [sops](https://github.com/mozilla/sops) with [age](https://github.com/FiloSottile/age) encryption for managing secrets across different environments.

## Architecture

The configuration system separates sensitive and non-sensitive data:

- **config/*.yaml** - Non-sensitive configuration (host names, ports, pool sizes, etc.)
- **secrets/*.yaml** - Sensitive data (passwords, API keys, etc.) encrypted with sops

At runtime, the application:
1. Loads the appropriate config YAML for the environment
2. Loads and decrypts the corresponding secrets YAML
3. Merges them into an AppConfig structure
4. Connects to databases and starts services

## Environments

The application supports four environments:

- **dev** - Local development (unencrypted secrets for convenience)
- **ci** - Continuous integration (encrypted with CI age key)
- **staging** - Staging deployment (encrypted with staging age key)
- **prod** - Production deployment (encrypted with production age key)

## Setting Up Secrets

### 1. Generate Age Keys

For each environment, generate an age key pair:

```bash
# Generate a key pair
age-keygen -o ~/.config/sops/age/keys.txt

# Or convert an existing Ed25519 SSH key
ssh-to-age < ~/.ssh/id_ed25519.pub
```

This outputs a public key like: `age1ql3z7hjy54pw3hyww5ayyfg7zqgvc7w3j2elw8zmrj2kg5sfn9aqmcac8p`

### 2. Update .sops.yaml

Edit `secrets/.sops.yaml` and replace the placeholder age keys with your real public keys:

```yaml
creation_rules:
  - path_regex: dev\.yaml$
    age: >-
      age1your_dev_public_key_here

  - path_regex: staging\.yaml$
    age: >-
      age1your_staging_public_key_here
```

### 3. Encrypt Secrets

For production and staging environments, encrypt the secrets files:

```bash
# Encrypt a secrets file
sops -e -i secrets/prod.yaml

# The file is now encrypted and safe to commit
```

For development, you can leave `secrets/dev.yaml` unencrypted for convenience, or encrypt it with your personal age key.

## Usage

### Running the Application

Specify the environment via CLI flag or environment variable:

```bash
# Via CLI flag
nix run .#hoard -- --env prod

# Via environment variable
export HOARD_ENV=staging
nix run .#hoard

# Default environment is 'dev'
nix run .#hoard
```

### Editing Secrets

Use the provided helper script to edit encrypted secrets:

```bash
# Edit production secrets
nix run .#secrets-edit prod

# This will:
# 1. Decrypt the file
# 2. Open it in your $EDITOR
# 3. Re-encrypt it when you save and exit
```

### Viewing Decrypted Secrets

To view decrypted secrets without editing:

```bash
# Decrypt to stdout
nix run .#secrets-decrypt prod
```

### Managing Keys

**Private keys** (used for decryption):
- **Local development**: Store in `~/.config/sops/age/keys.txt`
- **CI**: Store in CI secrets (GitHub Actions secrets, etc.)
- **Production**: Store in secure vault (HashiCorp Vault, AWS Secrets Manager, etc.)

**Public keys** (used for encryption):
- Stored in `secrets/.sops.yaml`
- Safe to commit to git

### Adding a New Secret

1. Edit the secrets file for your environment:
   ```bash
   nix run .#secrets-edit dev
   ```

2. Add the new secret following the YAML structure:
   ```yaml
   database:
     reader:
       user: hoard_reader
       password: your_password_here
     writer:
       user: hoard_writer
       password: another_password_here
   ```

3. If you add a new top-level section, update the Haskell types:
   - Add fields to `SecretConfig` in `src/Hoard/Types/AppConfig.hs`
   - Update the `FromJSON` instance
   - Update the config loading in `src/Hoard/Config/Loader.hs`

### Adding a New Environment

1. Create config file: `config/newenv.yaml`
2. Create secrets file: `secrets/newenv.yaml`
3. Generate an age key for the environment
4. Add encryption rule to `secrets/.sops.yaml`:
   ```yaml
   - path_regex: newenv\.yaml$
     age: >-
       age1your_newenv_public_key_here
   ```
5. Encrypt the secrets file: `sops -e -i secrets/newenv.yaml`
6. Add the environment to `Hoard.Types.Environment` module

## Security Best Practices

1. **Never commit unencrypted secrets** (except dev if you choose)
2. **Rotate age keys regularly** for production environments
3. **Use different keys per environment** - compromising one doesn't compromise all
4. **Store private keys securely**:
   - Use hardware security keys when possible
   - Restrict file permissions: `chmod 600 ~/.config/sops/age/keys.txt`
5. **Audit access** - track who has access to which age keys
6. **Use key rotation** - sops supports multiple keys, allowing gradual rotation

## Troubleshooting

### "no key could decrypt the data"

This means your private key doesn't match any of the public keys that encrypted the file.

**Solution**: Ensure you have the correct private key in `~/.config/sops/age/keys.txt` or set via `SOPS_AGE_KEY`

### "failed to get data key"

The `.sops.yaml` file isn't correctly configured for your secrets file.

**Solution**: Verify the `path_regex` pattern matches your file name

### Secrets not loading at runtime

The application looks for files in `config/` and `secrets/` relative to the current directory.

**Solution**: Run the application from the project root directory

## References

- [SOPS Documentation](https://github.com/mozilla/sops)
- [Age Encryption](https://age-encryption.org/)
- [sops-nix](https://github.com/Mic92/sops-nix) (for NixOS deployments)
