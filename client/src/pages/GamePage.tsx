import { useParams } from 'react-router-dom';

export function GamePage() {
  const { id } = useParams<{ id: string }>();
  return <div>Game: {id}</div>;
}
