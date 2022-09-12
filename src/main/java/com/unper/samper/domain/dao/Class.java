package com.unper.samper.domain.dao;

import com.unper.samper.domain.common.Auditor;
import lombok.*;

import javax.persistence.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
@Entity
@Table(name = "class", schema = "public")
public class Class extends Auditor {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = User.class)
    @JoinColumn(name = "user_id")
    private User lecture;

    @Column(name = "tittle", nullable = false)
    private String tittle;

    @Column(name = "subject", nullable = false)
    private String subject;
}
