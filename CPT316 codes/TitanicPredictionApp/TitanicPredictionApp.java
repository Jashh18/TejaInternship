import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;

public class TitanicPredictionApp {

    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            e.printStackTrace();
        }

        JFrame frame = new JFrame("Titanic Survival Predictor");
        frame.setSize(700, 600);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLocationRelativeTo(null);

        // Main panel with gradient background
        JPanel mainPanel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                Graphics2D g2d = (Graphics2D) g;
                g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
                int w = getWidth();
                int h = getHeight();
                Color color1 = new Color(25, 118, 210); // Dark blue
                Color color2 = new Color(100, 181, 246); // Light blue
                GradientPaint gp = new GradientPaint(0, 0, color1, w, h, color2);
                g2d.setPaint(gp);
                g2d.fillRect(0, 0, w, h);
            }
        };
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(30, 30, 30, 30));

        // Title Panel with fancy design
        JPanel titlePanel = new JPanel();
        titlePanel.setOpaque(false);
        JLabel titleLabel = new JLabel("Titanic Survival Prediction");
        titleLabel.setFont(new Font("Palatino", Font.BOLD, 36));
        titleLabel.setForeground(Color.WHITE);
        titleLabel.setBorder(BorderFactory.createEmptyBorder(0, 0, 30, 0));
        titlePanel.add(titleLabel);

        // Content Panel
        JPanel contentPanel = new JPanel();
        contentPanel.setOpaque(false);
        contentPanel.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(10, 10, 10, 10);

        // Add components with precise positioning
        placeComponents(contentPanel, gbc);

        // Add panels to main panel
        mainPanel.add(titlePanel);
        mainPanel.add(contentPanel);

        frame.add(mainPanel);
        frame.setVisible(true);
    }

    private static void placeComponents(JPanel panel, GridBagConstraints gbc) {
        String[] labels = {
            "Passenger Class (1-3):", 
            "Age:", 
            "Sex (male/female):", 
            "Fare:", 
            "Siblings/Spouse:", 
            "Parents/Children:"
        };
        JTextField[] fields = new JTextField[labels.length];

        // Create centered panel for form elements
        for (int i = 0; i < labels.length; i++) {
            gbc.gridx = 0;
            gbc.gridy = i;
            gbc.anchor = GridBagConstraints.EAST;
            JLabel label = createStyledLabel(labels[i]);
            panel.add(label, gbc);

            gbc.gridx = 1;
            gbc.anchor = GridBagConstraints.WEST;
            fields[i] = createStyledTextField();
            panel.add(fields[i], gbc);
        }

        // Create button and result panel
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.CENTER;
        
        JPanel buttonResultPanel = new JPanel();
        buttonResultPanel.setOpaque(false);
        buttonResultPanel.setLayout(new BoxLayout(buttonResultPanel, BoxLayout.Y_AXIS));

        JButton predictButton = createStyledButton("Predict Survival");
        predictButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        JLabel resultLabel = new JLabel("Awaiting prediction...");
        resultLabel.setFont(new Font("Georgia", Font.BOLD, 16));
        resultLabel.setForeground(Color.WHITE);
        resultLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        resultLabel.setBorder(BorderFactory.createEmptyBorder(20, 0, 0, 0));

        buttonResultPanel.add(predictButton);
        buttonResultPanel.add(resultLabel);
        panel.add(buttonResultPanel, gbc);

        predictButton.addActionListener(e -> {
            try {
                String inputData = String.format(
                    "{\"Pclass\":%s,\"Age\":%s,\"Sex\":\"%s\",\"Fare\":%s,\"SibSp\":%s,\"Parch\":%s}",
                    fields[0].getText(), fields[1].getText(), fields[2].getText(),
                    fields[3].getText(), fields[4].getText(), fields[5].getText()
                );

                URL url = new URL("http://127.0.0.1:8000/predict/");
                HttpURLConnection connection = (HttpURLConnection) url.openConnection();
                connection.setRequestMethod("POST");
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setDoOutput(true);

                try (OutputStream os = connection.getOutputStream()) {
                    os.write(inputData.getBytes(StandardCharsets.UTF_8));
                }

                Scanner scanner = new Scanner(connection.getInputStream());
                String response = scanner.useDelimiter("\\A").next();
                resultLabel.setText("Prediction: " + response);
                scanner.close();
            } catch (Exception ex) {
                resultLabel.setText("Error: " + ex.getMessage());
            }
        });
    }

    private static JLabel createStyledLabel(String text) {
        JLabel label = new JLabel(text);
        label.setFont(new Font("Georgia", Font.BOLD, 16));
        label.setForeground(Color.WHITE);
        return label;
    }

    private static JTextField createStyledTextField() {
        JTextField textField = new JTextField(15);
        textField.setFont(new Font("Georgia", Font.PLAIN, 14));
        textField.setBackground(new Color(240, 248, 255)); // Light blue-white
        textField.setForeground(new Color(25, 25, 112)); // Dark blue
        
        // Rounded border with padding
        Border lineBorder = BorderFactory.createLineBorder(Color.WHITE, 2, true);
        Border paddingBorder = BorderFactory.createEmptyBorder(8, 10, 8, 10);
        textField.setBorder(BorderFactory.createCompoundBorder(lineBorder, paddingBorder));
        
        return textField;
    }

    private static JButton createStyledButton(String text) {
        JButton button = new JButton(text);
        button.setFont(new Font("Georgia", Font.BOLD, 16));
        button.setForeground(Color.BLACK); // Changed to black text
        button.setBackground(new Color(255, 215, 0)); // Changed to gold color
        button.setFocusPainted(false);
        button.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(new Color(218, 165, 32), 2, true), // Darker gold border
            BorderFactory.createEmptyBorder(10, 25, 10, 25)
        ));
        
        // Hover effect with new colors
        button.addMouseListener(new MouseAdapter() {
            public void mouseEntered(MouseEvent e) {
                button.setBackground(new Color(255, 200, 0)); // Slightly darker on hover
            }
            public void mouseExited(MouseEvent e) {
                button.setBackground(new Color(255, 215, 0)); // Back to original gold
            }
        });
        
        return button;
    }
}