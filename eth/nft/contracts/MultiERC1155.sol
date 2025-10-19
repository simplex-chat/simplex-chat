// SPDX-License-Identifier: MIT
pragma solidity ^0.8.27;

import "@openzeppelin/contracts@5.4.0/token/ERC1155/ERC1155.sol";
import "@openzeppelin/contracts@5.4.0/access/Ownable.sol";
import "@openzeppelin/contracts@5.4.0/utils/Base64.sol";
import "@openzeppelin/contracts@5.4.0/utils/Strings.sol";

/// @title MultiERC1155
/// @notice ERC1155 contract with sequential variants for immutable metadata.
/// @dev Non-upgradeable. Admin and minter addresses are settable by owner. Global sequential token IDs.
contract MultiERC1155 is ERC1155, Ownable {
    address public admin; // can manage token IDs
    address public minter; // can mint tokens for existing IDs
    bool public mintingEnabled;
    bool public contractLocked; // no more variants can be added, minting cannot be enabled, cannot be unlocked

    // for name and description avoid or escape double quotes, so it can be used in JSON
    struct TokenInfo {
        string tokenUri;
        uint totalSupply; // 0 for unlimited
        bool enabled;
    }

    struct TokenState {
        TokenInfo tokenInfo;
        uint currentSupply;
        bool exists;
        bool locked;
    }

    uint private _nextTokenId; // Global sequential token ID (starts at 1)
    mapping(uint => TokenState) public tokens;
    uint[] public tokenIds;

    function getTokenIds() view external returns(uint[] memory) {
        return tokenIds;
    }

    event AdminUpdated(address indexed newAdmin);
    event MinterUpdated(address indexed newMinter);
    event MintingEnabled(bool newEnabled);
    event ContractLocked();
    event TokenAdded(uint indexed tokenId);
    event TokenRemoved(uint indexed tokenId);
    event TokenUpdated(uint indexed tokenId, bool newEnabled, uint newTotalSupply);
    event TokenLocked(uint indexed tokenId);

    constructor() ERC1155("") Ownable(msg.sender) {
        admin = msg.sender;
        minter = msg.sender;
        _nextTokenId = 1;
        mintingEnabled = true;
    }

    /// @notice Updates the minter address.
    /// @param newAdmin The new minter.
    function setAdmin(address newAdmin) external onlyOwner {
        admin = newAdmin;
        emit AdminUpdated(newAdmin);
    }

    /// @notice Updates the minter address.
    /// @param newMinter The new minter.
    function setMinter(address newMinter) external onlyOwner {
        minter = newMinter;
        emit MinterUpdated(newMinter);
    }

    /// @notice Enables/disables minting.
    /// @param enabled True/false to enable/disable minting.
    function toggleMinting(bool enabled) external onlyOwner {
        if (mintingEnabled != enabled) {
            mintingEnabled = enabled;
            emit MintingEnabled(enabled);
        }
    }

    /// @notice Permanently locks any token changes and minting, irreversible.
    function lockContract() external onlyOwner {
        contractLocked = true;
        mintingEnabled = false;
        emit ContractLocked();
    }

    /// @notice Adds a new variant and optionally sets it as current.
    /// @param info New token info.
    function addToken(TokenInfo memory info) external {
        require(msg.sender == admin || msg.sender == owner(), "Only admin and owner can add tokens");
        _addToken(info);
    }

    function _addToken(TokenInfo memory info) internal {
        require(bytes(info.tokenUri).length > 0, "Token tokenUri required");
        
        uint id = _nextTokenId++;
        require(!tokens[id].exists, "Contract error: token ID already exists");

        tokens[id] = TokenState({
            tokenInfo: info,
            currentSupply: 0,
            exists: true,
            locked: false
        });
        tokenIds.push(id);

        emit TokenAdded(id);
    }

    /// @notice Removes the last variant if unused (currentSupply == 0).
    function removeToken(uint id) external {
        require(msg.sender == admin || msg.sender == owner(), "Only admin and owner can remove tokens");
        TokenState storage token = tokens[id];
        require(token.exists, "Token ID does not exist");
        require(token.currentSupply == 0, "Tokens already minted for this ID");
        require(tokenIds.length > 1, "Cannot remove the last token ID");

        delete tokens[id];
        for (uint i = 0; i < tokenIds.length; i++) {
            if (tokenIds[i] == id) {
                tokenIds[i] = tokenIds[tokenIds.length - 1];
                tokenIds.pop();
                break;
            }
        }
        emit TokenRemoved(id);
    }


    /// @notice Enables/disables minting a specific token.
    /// @param id Token ID.
    /// @param newEnabled True/false to enable/disable minting.
    /// @param newTotalSupply 0 for unlimited
    function updateToken(uint id, bool newEnabled, uint newTotalSupply) external {
        require(msg.sender == admin || msg.sender == owner(), "Only admin and owner can remove tokens");
        TokenState storage token = tokens[id];
        require(token.exists, "Token ID does not exist");
        require(!token.locked, "Token ID is locked");
        require(newTotalSupply == 0 || token.currentSupply <= newTotalSupply, "New total supply must be greater than existing token count for ID");

        if (token.tokenInfo.enabled != newEnabled || token.tokenInfo.totalSupply != newTotalSupply) {
            token.tokenInfo.enabled = newEnabled;
            token.tokenInfo.totalSupply = newTotalSupply;
            emit TokenUpdated(id, newEnabled, newTotalSupply);
        }
    }

    /// @notice permanently lock a specific token from any further changes.
    /// @param id Token ID.
    function lockToken(uint id) external onlyOwner {
        TokenState storage token = tokens[id];
        require(token.exists, "Token ID does not exist");
        require(!token.locked, "Token ID is already locked");
        require(token.currentSupply != 0, "No tokens minted for this ID, use removeToken instead");
        token.locked = true;
        token.tokenInfo.enabled = false;
        emit TokenLocked(id);
    }

    /// @notice Mints a token using the default variant.
    /// @param to Recipient.
    /// @param id Token ID.
    /// @param value Amount to mint.
    /// @param data Optional data.
    function mint(address to, uint256 id, uint256 value, bytes calldata data) external {
        require(!contractLocked, "Contract is permanently locked");
        require(mintingEnabled, "Minting is disabled");
        require(msg.sender == minter || msg.sender == admin || msg.sender == owner(), "Only minter, admin or owner can mint");
        TokenState storage token = tokens[id];
        require(token.exists, "Token ID does not exist");
        require(!token.locked, "Token ID is locked");
        require(token.tokenInfo.enabled, "Token ID is disabled");
        require(token.tokenInfo.totalSupply == 0 || token.tokenInfo.totalSupply >= token.currentSupply + value, "Token supply exceeded for this ID");
        require(value > 0, "Amount must be > 0");

        _mint(to, id, value, data);
    }

    /// @dev Hook to update supplies on transfer/burn.
    function _update(address from, address to, uint[] memory ids, uint[] memory values) internal virtual override {
        super._update(from, to, ids, values);

        for (uint i = 0; i < ids.length; i++) {
            if (from == address(0)) { // mint
                tokens[ids[i]].currentSupply += values[i];
            }
            if (to == address(0)) { // burn
                tokens[ids[i]].currentSupply -= values[i];
            }
        }
    }

    /// @notice Returns embedded JSON metadata URI.
    /// @param id Token ID.
    function uri(uint id) public view virtual override returns (string memory) {
        TokenState storage token = tokens[id];
        require(token.exists, "Invalid token ID");
        return token.tokenInfo.tokenUri;
    }
}
