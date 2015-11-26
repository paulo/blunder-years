import co.paralleluniverse.actors.*;
import co.paralleluniverse.fibers.SuspendExecution;
import java.util.concurrent.TimeUnit;

public class Ring {
    static final int N = 1000;
    static final int M = 1000;

    public static void main(String args[]) throws Exception {
        for (int i = 0; i < 5; i++)
            new Ring().run();
    }

    void run() throws Exception {
        final long start = System.nanoTime();

        Actor<Integer, Integer> manager = new BasicActor<Integer, Integer>() {
            @Override
            protected Integer doRun() throws InterruptedException, SuspendExecution {
                ActorRef<Integer> a = this.ref();
                //criar 1000 actores cuja funçao é simplesmente enviar para o actor anterior o inteiro que receberam incrementado em 1
                //enquanto nao se manda nada, os actores estao bloqueados, com o receive
                for (int i = 0; i < N - 1; i++)
                    a = spawnRelay(a);
                //envia-se o inteiro para o ultimo actor criado
                a.send(1);
                Integer msg = null;
                //finalmente, como o manager é o 1ºactor criado, faz-se receive da mensagem destinada a ele, e envia-se novamente
                //para o ultimo actor criado o incremento. Isto dura mais 1000 ciclos. O resultado final deve ser 1000 * 1000
                for (int i = 0; i < M; i++) {
                    msg = receive();
                    a.send(msg + 1);
                }
                return msg;
            }
        };

        manager.spawn();
        int totalCount = manager.get();
        final long time = TimeUnit.MILLISECONDS.convert(System.nanoTime() - start, TimeUnit.NANOSECONDS);
        System.out.println("messages: " + totalCount + " time (ms): " + time);
    }

    //este actor simplesmente recebe um inteiro e envia-o para o actor anteriormente criado
    private ActorRef<Integer> spawnRelay(final ActorRef<Integer> prev) {
        return new BasicActor<Integer, Void>() {
            @Override
            protected Void doRun() throws InterruptedException, SuspendExecution {
                for (;;) {
                    Integer m = receive();
                    prev.send(m + 1);
                }
            }
        }.spawn();
    }
}
