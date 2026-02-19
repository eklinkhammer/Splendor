import React from 'react';
import ReactDOM from 'react-dom/client';
import { BrowserRouter, Routes, Route } from 'react-router-dom';
import './index.css';
import { HomePage } from './pages/HomePage';
import { GamePage } from './pages/GamePage';
import { LobbyPage } from './pages/LobbyPage';

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<HomePage />} />
        <Route path="/lobby/:lobbyId" element={<LobbyPage />} />
        <Route path="/game/:id" element={<GamePage />} />
        <Route path="/game/:id/spectate" element={<GamePage />} />
      </Routes>
    </BrowserRouter>
  </React.StrictMode>,
);
